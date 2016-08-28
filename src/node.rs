// use std::collections::LinkedList;
use std::fmt;
use std::hash::Hash;
use std::hash::BuildHasher;
use std::ops::Deref;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::{Acquire, Release};

use crossbeam::mem::epoch::{self, Atomic, Owned};

use ctrie::CTrie;
use persistent_list::PersistentList;

#[derive(Clone)]
pub struct Gen(usize);

impl Gen {
    pub fn new() -> Arc<Gen> {
        Arc::new(Gen(0))
    }
}

pub type ArcGen = Arc<Gen>;

pub fn ptr_eq<T>(first: &T, second: &T) -> bool {
    first as *const T == second as *const T
}

#[derive(Clone, Debug)]
pub struct RdcssDescriptor<K, V> where K: Clone, V: Clone {
    pub old: INode<K, V>,
    pub expected: MainNodeStruct<K, V>,
    pub nv: INode<K, V>,
    pub committed: Arc<AtomicBool>,
}

#[derive(Clone)]
pub struct MainNodeStruct<K: Clone, V: Clone>(pub MainNode<K, V>, pub Arc<Atomic<MainNodeStruct<K, V>>>);

impl<K: Clone + fmt::Debug, V: Clone + fmt::Debug> fmt::Debug for MainNodeStruct<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.debug_struct("MainNodeStruct")
            .field("node", &self.0)
            .finish()
    }
}

impl<K: Clone, V: Clone> PartialEq for MainNodeStruct<K, V> {
    fn eq(&self, other: &Self) -> bool {
        // TODO
        ptr_eq(self.1.as_ref(), other.1.as_ref())
    }
}

impl<K: Clone, V: Clone> Eq for MainNodeStruct<K, V> {}

#[derive(Clone)]
pub enum MainNode<K: Clone, V: Clone> {
    CTrieNode(CNode<K, V>),
    List(LNode<K, V>),
    Tomb(TNode<K, V>),
    Failed(Box<MainNodeStruct<K, V>>),
}

impl<K: Clone + fmt::Debug, V: Clone + fmt::Debug> fmt::Debug for MainNode<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            MainNode::CTrieNode(ref cnode) => cnode.fmt(f),
            MainNode::List(ref lnode) => lnode.fmt(f),
            MainNode::Tomb(ref tnode) => tnode.fmt(f),
            MainNode::Failed(_) => f.debug_struct("FAILED").finish()
        }
    }
}

#[derive(Debug)]
pub enum Branch<K: Clone, V: Clone> {
    Indirection(INode<K, V>),
    Singleton(SNode<K, V>),
}

#[derive(Clone)]
pub struct INode<K: Clone, V: Clone> {
    // TODO is this arc right?
    pub node: Arc<Atomic<MainNodeStruct<K, V>>>,
    pub gen: Arc<Gen>,
    pub rdcss: Option<Box<RdcssDescriptor<K, V>>>,
}

// TODO fix?
impl<K: Clone + fmt::Debug, V: Clone + fmt::Debug> fmt::Debug for INode<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let guard = epoch::pin();
        f.debug_struct("INode")
            .field("node", *self.node.load(Acquire, &guard).unwrap())
            .finish()
    }
}

impl<K: Clone, V: Clone> INode<K, V> {
    pub fn new(node: MainNode<K, V>) -> INode<K, V> {
        INode {
            node: Arc::new(
                Atomic::new(
                    MainNodeStruct(node, Arc::new(Atomic::null()))
                )
            ),
            gen: Gen::new(),
            rdcss: None,
        }
    }
}

impl<K: Clone + Hash + Eq, V: Clone> INode<K, V> {
    pub fn copy_to_gen<H: BuildHasher + Clone>(&self, ctrie: &CTrie<K, V, H>, gen: Arc<Gen>) -> INode<K, V> {
        let new_inode: INode<K, V> = INode {
            node: Arc::new(Atomic::null()),
            gen: gen,
            rdcss: None,
        };
        let guard = epoch::pin();
        let main = ctrie.gcas_read(self, &guard);
        new_inode.node.store(Some(Owned::new((**main.unwrap()).clone())), Release);
        new_inode
    }
}

impl<K: Clone, V: Clone> PartialEq for INode<K, V> {
    fn eq(&self, other: &Self) -> bool {
        // TODO right?
        // (self.node.as_ref() as *const Atomic<MainNodeStruct<K, V>>) ==
        //     (other.node.as_ref() as *const Atomic<MainNodeStruct<K, V>>)
        ptr_eq(self.node.as_ref(), other.node.as_ref())
    }
}

impl<K: Clone, V: Clone> Eq for INode<K, V> {}

#[derive(Debug, Clone)]
pub struct CNode<K: Clone, V: Clone> {
    // bitmap and array
    pub bmp: u32,
    pub array: Vec<Arc<Branch<K, V>>>,
}

#[derive(Debug, Clone)]
pub struct LNode<K: Clone, V: Clone> {
    pub nodes: PersistentList<SNode<K, V>>,
}

#[derive(Debug, Clone)]
pub struct TNode<K: Clone, V: Clone> {
    pub singleton: SNode<K, V>,
}

impl<K: Clone, V: Clone> TNode<K, V> {
    pub fn resurrect(self) -> SNode<K, V> {
        self.singleton
    }
}

#[derive(Clone, Debug)]
pub struct SNode<K: Clone, V: Clone> {
    pub key: K,
    pub value: V,
}

impl<K: Clone + Hash + Eq, V: Clone> CNode<K, V> {
    pub fn new() -> CNode<K, V> {
        CNode {
            bmp: 0,
            array: Vec::new(),
        }
    }

    pub fn inserted(pos: u32, flag: u32, cnode: &CNode<K, V>, key: K, value: V) -> CNode<K, V> {
        if cnode.bmp == cnode.bmp | flag {
            println!("same bitmap");
        }
        let bmp = cnode.bmp | flag;

        let snode = SNode {
            key: key,
            value: value,
        };
        let mut array = cnode.array.clone();
        if array.len() > 0 && !ptr_eq(array[0].as_ref(), cnode.array[0].as_ref()) {
            println!("SHIT SHIT SHIT");
        }
        // println!("cnode inserting key: {:?}, array: {:?}", key, array);
        array.insert(pos as usize, Arc::new(Branch::Singleton(snode)));

        CNode {
            bmp: bmp,
            array: array,
        }
    }

    pub fn updated(pos: u32, cnode: &CNode<K, V>, branch: Branch<K, V>) -> CNode<K, V> {
        let mut array = cnode.array.clone();
        array[pos as usize] = Arc::new(branch);

        CNode {
            bmp: cnode.bmp,
            array: array,
        }
    }

    pub fn removed(pos: u32, flag: u32, cnode: &CNode<K, V>) -> CNode<K, V> {
        let mut array = cnode.array.clone();
        array.remove(pos as usize);

        CNode {
            bmp: cnode.bmp ^ flag,
            array: array,
        }
    }

    pub fn renewed<H: BuildHasher + Clone>(&self, ctrie: &CTrie<K, V, H>, gen: Arc<Gen>) -> CNode<K, V> {
        // TODO clone needed?
        let array = self.array.clone().iter().map(|branch| {
            match **branch {
                // TODO
                Branch::Indirection(ref inode) => Arc::new(Branch::Indirection(inode.copy_to_gen(ctrie, gen.clone()))),
                // Branch::Singleton(ref snode) => Box::new(Branch::Singleton(snode.clone())),
                Branch::Singleton(_) => branch.clone(),
            }
        }).collect::<Vec<Arc<Branch<K, V>>>>();

        CNode {
            bmp: self.bmp,
            array: array,
        }
    }
}

impl<K: Clone + Eq, V: Clone> LNode<K, V> {
    pub fn new() -> LNode<K, V> {
        LNode {
            nodes: PersistentList::new(),
        }
    }

    pub fn lookup(&self, key: &K) -> Option<V> {
        self.nodes.iter().find(|arc| {
            arc.deref().key == *key
        }).map(|v| v.value.clone())
    }

    pub fn inserted(lnode: &LNode<K, V>, key: K, value: V) -> LNode<K, V> {
        LNode {
            nodes: lnode.nodes.prepend(SNode {
                key: key,
                value: value,
            }),
        }
    }

    pub fn removed(lnode: &LNode<K, V>, key: &K) -> LNode<K, V> {
        // TODO make this a function in persistent_list?
        let opt_idx = lnode.nodes.iter().position(|arc| {
            arc.key == *key
        });

        match opt_idx {
            Some(idx) => LNode {
                nodes: lnode.nodes.remove(idx),
            },
            None => lnode.clone(),
        }
    }
}
