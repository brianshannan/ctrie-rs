use std::fmt;
use std::hash::BuildHasher;
use std::hash::BuildHasherDefault;
use std::hash::Hash;
use std::hash::Hasher;
use std::hash::SipHasher;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering::{Acquire, Release};

use crossbeam::mem::epoch::{self, Atomic, Owned, Shared, Guard};
// use thread_local::ThreadLocal;

use iter::CTrieIter;
use persistent_list::PersistentList;
use node::ptr_eq;
use node::INode;
use node::CNode;
use node::LNode;
use node::TNode;
use node::SNode;
use node::MainNode;
use node::MainNodeStruct;
use node::Branch;
use node::RdcssDescriptor;
use node::Gen;

// TODO see if the iterator can work without clones
// TODO memory leaks everywhere
// TODO change tail recursion to loops
// TODO reduce number of clone necessary
// TODO gcas and rdcss are both a mess

pub type DefaultHashBuilder = BuildHasherDefault<SipHasher>;

pub type ArcCTrie<K, V, H> = CTrie<Arc<K>, Arc<V>, H>;

pub struct CTrie<K, V, H=DefaultHashBuilder> where K: Hash + Eq + Clone, V: Clone, H: BuildHasher + Clone {
    root: Atomic<INode<K, V>>,
    read_only: bool,
    hash_builder: H,
    // hash_builder: ThreadLocal<H>,

}

impl <K, V, H> fmt::Debug for CTrie<K, V, H> where K: Hash + Eq + Clone + fmt::Debug, V: Clone + fmt::Debug, H: BuildHasher + Clone {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let guard = epoch::pin();
        // TODO fix
        f.debug_struct("CTrie")
            .field("root", *self.root.load(Acquire, &guard).unwrap())
            .finish()
    }
}

impl<K, V> CTrie<K, V, DefaultHashBuilder> where K: Hash + Eq + Clone, V: Clone {
    pub fn new() -> CTrie<K, V> {
        CTrie::with_hasher(DefaultHashBuilder::default())
    }
}

impl<K, V, H> CTrie<K, V, H> where K: Hash + Eq + Clone, V: Clone, H: BuildHasher + Clone {
    pub fn with_hasher(hash_builder: H) -> CTrie<K, V, H> {
        CTrie {
            root: Atomic::new(
                INode {
                    node: Arc::new(
                        Atomic::new(
                            MainNodeStruct(MainNode::CTrieNode(
                                CNode::new()
                            ), Arc::new(Atomic::null()))
                        )
                    ),
                    gen: Gen::new(),
                    rdcss: None,
                }
            ),
            read_only: false,
            hash_builder: hash_builder,
        }
    }

    fn gcas_complete<'a>(&self, inode: &INode<K, V>, mnode: Option<Shared<'a, MainNodeStruct<K, V>>>, guard: &'a Guard) -> Option<Shared<'a, MainNodeStruct<K, V>>> {
        if mnode.is_none() {
            return None;
        }

        let mn = mnode.unwrap();
        let prev = mn.1.load(Acquire, guard);
        if prev.is_none() {
            return mnode;
        }
        let root = self.rdcss_read_root(true, guard);

        // TODO fix?
        match **prev.unwrap() {
            MainNodeStruct(MainNode::Failed(ref fnode), _) => {
                // gcas failure, try to commit previous value
                match inode.node.cas_and_ref(mnode, Owned::new(*fnode.clone()), Release, guard) {
                    Ok(shared) => Some(shared),
                    Err(_) => self.gcas_complete(inode, inode.node.load(Acquire, guard), guard),
                }
            },
            MainNodeStruct(_, _) => {
                if ptr_eq(root.gen.as_ref(), inode.gen.as_ref()) && !self.read_only {
                    // try to commit
                    match mn.1.cas(prev, None, Release) {
                        Ok(()) =>  Some(mn),
                        Err(_) => {
                            self.gcas_complete(inode, mnode, guard)
                        },
                    }
                } else {
                    // try to abort
                    let new_failed = MainNodeStruct(MainNode::Failed(Box::new((**prev.unwrap()).clone())), Arc::new(Atomic::null()));
                    let _ = mn.1.cas(prev, Some(Owned::new(new_failed)), Release);
                    self.gcas_complete(inode, inode.node.load(Acquire, guard), guard)
                }
            },
        }
    }

    fn gcas(&self, inode: &INode<K, V>, old: Option<Shared<MainNodeStruct<K, V>>>, n: MainNodeStruct<K, V>) -> bool {
        let guard = epoch::pin();
        n.1.store_shared(old, Release);
        match inode.node.cas_and_ref(old, Owned::new(n), Release, &guard) {
            Ok(shared) => {
                self.gcas_complete(inode, Some(shared), &guard);
                let success = shared.1.load(Acquire, &guard).is_none();
                success
            },
            Err(_) => {
                false
            },
        }
    }

    // TODO make not public
    pub fn gcas_read<'a>(&self, inode: &INode<K, V>, guard: &'a Guard) -> Option<Shared<'a, MainNodeStruct<K, V>>> {
        let m = inode.node.load(Acquire, guard);
        match m {
            Some(shared) => match shared.1.load(Acquire, &guard) {
                Some(_) => self.gcas_complete(inode, m, guard),
                None => m,
            },
            None => m,
        }
    }

    fn rdcss_complete<'a>(&self, abort: bool, guard: &'a Guard) -> Shared<'a, INode<K, V>> {
        let r = self.root.load(Acquire, &guard);

        match (*r.unwrap()).rdcss {
            Some(ref rdcss) => {
                if abort {
                    // TODO
                    return match self.root.cas_and_ref(r, Owned::new(rdcss.old.clone()), Release, guard) {
                        Ok(shared) => shared,
                        Err(_) => self.rdcss_complete(abort, guard),
                    };
                }

                // TODO
                let old_main = self.gcas_read(&rdcss.old, &guard);
                if **old_main.unwrap() == rdcss.expected.clone() {
                    // commit the rdcss
                    match self.root.cas_and_ref(r, Owned::new(rdcss.nv.clone()), Release, guard) {
                        Ok(shared) => {
                            rdcss.committed.store(true, Release);
                            shared
                        },
                        Err(_) => self.rdcss_complete(abort, guard),
                    }
                } else {
                    // abort
                    match self.root.cas_and_ref(r, Owned::new(rdcss.old.clone()), Release, guard) {
                        Ok(shared) => shared,
                        Err(_) => self.rdcss_complete(abort, guard),
                    }
                }
            },
            None => {
                r.unwrap()
            },
        }
    }

    pub fn rdcss_read_root<'a>(&self, abort: bool, guard: &'a Guard) -> Shared<'a, INode<K, V>> {
        // TODO right?
        let r = self.root.load(Acquire, guard);
        let inode = *r.unwrap();
        if inode.rdcss.is_some() {
            self.rdcss_complete(abort, guard)
        } else {
            r.unwrap()
        }
    }

    fn rdcss_root(&self, old: Shared<INode<K, V>>, expected: MainNodeStruct<K, V>, nv: INode<K, V>) -> bool {
        // TODO right?
        let guard = epoch::pin();
        let desc = RdcssDescriptor {
            old: (**old).clone(),
            expected: expected,
            nv: nv,
            committed: Arc::new(AtomicBool::new(false)),
        };
        let inode = INode {
            node: Arc::new(Atomic::null()),
            gen: Gen::new(),
            rdcss: Some(Box::new(desc.clone())),
        };

        match self.root.cas(Some(old), Some(Owned::new(inode)), Release) {
            Ok(()) => {
                self.rdcss_complete(false, &guard);
                desc.committed.load(Acquire)
            },
            Err(_) => false,
        }
    }

    pub fn snapshot(&self) -> CTrie<K, V, H> {
        // TODO right?
        let guard = epoch::pin();
        let r = self.rdcss_read_root(false, &guard);
        let main = self.gcas_read(*r, &guard);
        if self.rdcss_root(r, (**main.unwrap()).clone(), r.copy_to_gen(self, Gen::new())) {
            CTrie {
                root: Atomic::new(r.copy_to_gen(self, Gen::new())),
                read_only: self.read_only,
                hash_builder: self.hash_builder.clone(),
            }
        } else {
            self.snapshot()
        }
    }

    pub fn read_only_snapshot(&self) -> CTrie<K, V, H> {
        // TODO right?
        let guard = epoch::pin();
        let r = self.rdcss_read_root(false, &guard);
        let main = self.gcas_read(*r, &guard);
        if self.rdcss_root(r, (**main.unwrap()).clone(), r.copy_to_gen(self, Gen::new())) {
            CTrie {
                root: Atomic::new((**r).clone()),
                read_only: true,
                hash_builder: self.hash_builder.clone(),
            }
        } else {
            self.read_only_snapshot()
        }
    }

    pub fn size(&self) -> usize {
        // TODO optimize?
        self.iter().count()
    }

    // TODO automically create read only snapshot if not given one
    pub fn iter(&self) -> CTrieIter<K, V, H> {
        assert!(self.read_only);

        CTrieIter::new(self)
    }

    pub fn lookup(&self, key: &K) -> Option<V> {
        let guard = epoch::pin();
        let r = self.rdcss_read_root(false, &guard);
        match self.ilookup(&r, key, 0, None, &guard, r.gen.clone()) {
            Ok(result) => result,
            Err(()) => self.lookup(key),
        }
    }

    fn ilookup(&self, inode: &INode<K, V>, key: &K, lev: usize, parent: Option<&INode<K, V>>, guard: &Guard, startgen: Arc<Gen>) -> Result<Option<V>, ()> {
        let asdf = inode.node.load(Acquire, guard);
        match asdf {
            Some(main_node) => {
                match main_node.0 {
                    MainNode::CTrieNode(ref cnode) => {
                        let (flag, pos) = self.flag_position(self.hash_key(key), lev, cnode.bmp);
                        if cnode.bmp & flag == 0 {
                            return Ok(None);
                        }

                        match *cnode.array[pos as usize] {
                            Branch::Indirection(ref child_inode) => {
                                if self.read_only || ptr_eq(startgen.as_ref(), child_inode.gen.as_ref()) {
                                    return self.ilookup(child_inode, key, lev + 5, Some(inode), guard, startgen)
                                } else {
                                    let new_mnode = MainNodeStruct(
                                        MainNode::CTrieNode(cnode.renewed(self, startgen.clone())), Arc::new(Atomic::null())
                                    );

                                    if self.gcas(inode, asdf, new_mnode) {
                                        return self.ilookup(inode, key, lev, parent, guard, startgen)
                                    } else {
                                        return Err(())
                                    }
                                }
                            },
                            Branch::Singleton(ref snode) => {
                                if snode.key == *key {
                                    Ok(Some(snode.value.clone()))
                                } else {
                                    Ok(None)
                                }
                            },
                        }
                    },
                    MainNode::Tomb(ref tnode) => {
                        if self.read_only {
                            // TODO also check hashcode?
                            if tnode.singleton.key == *key {
                                return Ok(Some(tnode.singleton.value.clone()))
                            } else {
                                return Ok(None)
                            }
                        } else {
                            self.clean(parent.unwrap(), lev - 5);
                            return Err(())
                        }
                    },
                    MainNode::List(ref lnode) => Ok(lnode.lookup(key)),
                    MainNode::Failed(_) => unreachable!("not good"),
                }
            },
            None => Ok(None),
        }
    }

    pub fn insert(&self, key: K, value: V) -> () {
        assert!(!self.read_only);

        let guard = epoch::pin();
        let r = self.rdcss_read_root(false, &guard);
        // TODO clones
        if let Ok(result) = self.iinsert(&r, key.clone(), value.clone(), 0, None, &guard, r.gen.clone()) {
            result
        } else {
            // retry
            self.insert(key, value)
        }
    }

    fn iinsert(&self, inode: &INode<K, V>, key: K, value: V, lev: usize, parent: Option<&INode<K, V>>, guard: &Guard, startgen: Arc<Gen>) -> Result<(), ()> {
        let asdf = self.gcas_read(inode, guard);
        match asdf {
            Some(main_node) => {
                match main_node.0 {
                    MainNode::CTrieNode(ref cnode) => {
                        let (flag, pos) = self.flag_position(self.hash_key(&key), lev, cnode.bmp);
                        if cnode.bmp & flag == 0 {
                            // TODO check gen?
                            let new_cnode = MainNodeStruct(MainNode::CTrieNode(
                                CNode::inserted(pos, flag, &cnode, key, value)
                            ), Arc::new(Atomic::null()));

                            return match self.gcas(inode, asdf, new_cnode) {
                                true => Ok(()),
                                false => Err(()),
                            };
                        }

                        match *cnode.array[pos as usize] {
                            Branch::Indirection(ref new_inode) => {
                                if ptr_eq(new_inode.gen.as_ref(), startgen.as_ref()) {
                                    return self.iinsert(new_inode, key, value, lev + 5, Some(inode), guard, startgen);
                                } else {

                                    let new_main_node = MainNodeStruct(
                                        MainNode::CTrieNode(cnode.renewed(self, startgen.clone())), Arc::new(Atomic::null())
                                    );
                                    if self.gcas(inode, asdf, new_main_node) {
                                        return self.iinsert(inode, key, value, lev, parent, guard, startgen);
                                    } else {
                                        return Err(());
                                    }
                                }
                            },
                            Branch::Singleton(ref snode) => {
                                if snode.key == key {
                                    // TODO
                                    let new_cnode = MainNodeStruct(MainNode::CTrieNode(
                                        CNode::updated(pos, &cnode, Branch::Singleton(SNode {
                                            key: key.clone(),
                                            value: value,
                                        }))
                                    ), Arc::new(Atomic::null()));

                                    return match self.gcas(inode, asdf, new_cnode) {
                                        true => Ok(()),
                                        false => Err(()),
                                    };
                                } else {
                                    // TODO check gen equality?
                                    // need to extend tree due to a hash code collision
                                    let new_mnode = self.new_main_node(snode.clone(), SNode {
                                        key: key,
                                        value: value,
                                    }, lev + 5);

                                    let new_node = Branch::Indirection(
                                        INode {
                                            node: Arc::new(
                                                Atomic::new(
                                                    MainNodeStruct(new_mnode, Arc::new(Atomic::null()))
                                                )
                                            ),
                                            gen: inode.gen.clone(),
                                            rdcss: None,
                                        }
                                    );

                                    let new_mnode_struct = MainNodeStruct(
                                        MainNode::CTrieNode(CNode::updated(pos, cnode, new_node)), Arc::new(Atomic::null())
                                    );
                                    return match self.gcas(inode, asdf, new_mnode_struct) {
                                        true => Ok(()),
                                        false => Err(()),
                                    };
                                }
                            },
                        };
                    },
                    MainNode::List(ref lnode) => {
                        let new_mnode = MainNodeStruct(
                            MainNode::List(LNode::inserted(lnode, key, value)), Arc::new(Atomic::null())
                        );

                        return match self.gcas(inode, asdf, new_mnode) {
                            true => Ok(()),
                            false => Err(()),
                        };
                    },
                    MainNode::Tomb(_) => {
                        self.clean(parent.unwrap(), lev - 5);
                        return Err(());
                    },
                    MainNode::Failed(_) => unreachable!("shit"),
                };
            },
            None => {},
        };


        Err(())
    }

    fn new_main_node(&self, x: SNode<K, V>, y: SNode<K, V>, lev: usize) -> MainNode<K, V> {
        if lev > 64 {
            return MainNode::List(LNode {
                nodes: PersistentList::new()
                    .prepend(x)
                    .prepend(y),
            });
        }

        let xhc = self.hash_key(&x.key);
        let yhc = self.hash_key(&y.key);

        let xidx = (xhc >> lev) & 0x1f;
        let yidx = (yhc >> lev) & 0x1f;
        let bmp = (1 << xidx) | (1 << yidx);
        if xidx == yidx {
            // still have a hash collision, recurse
            let inode = INode::new(self.new_main_node(x, y, lev + 5));
            MainNode::CTrieNode(CNode {
                bmp: bmp,
                array: vec![Arc::new(Branch::Indirection(inode))],
            })
        } else {
            let sx = Arc::new(Branch::Singleton(x));
            let sy = Arc::new(Branch::Singleton(y));
            let array = if xidx < yidx {
                vec![sx, sy]
            } else {
                vec![sy, sx]
            };
            MainNode::CTrieNode(CNode {
                bmp: bmp,
                array: array,
            })
        }
    }

    // TODO this leaks and should return the value
    pub fn remove(&self, key: &K) -> Option<V> {
        assert!(!self.read_only);

        let guard = epoch::pin();
        let r = self.rdcss_read_root(false, &guard);
        match self.iremove(&r, key, 0, None, &guard, r.gen.clone()) {
            Ok(value) => value,
            Err(()) => self.remove(key),
        }
    }

    fn iremove(&self, inode: &INode<K, V>, key: &K, lev: usize, parent: Option<&INode<K, V>>, guard: &Guard, startgen: Arc<Gen>) -> Result<Option<V>, ()> {
        let asdf = self.gcas_read(inode, guard);
        match asdf {
            Some(main_node) => {
                match main_node.0 {
                    MainNode::CTrieNode(ref cnode) => {
                        let (flag, pos) = self.flag_position(self.hash_key(key), lev, cnode.bmp);
                        if cnode.bmp & flag == 0 {
                            // not present in array
                            return Ok(None);
                        }

                        match *cnode.array[pos as usize] {
                            Branch::Indirection(ref new_inode) => {
                                // TODO fix
                                if ptr_eq(new_inode.gen.as_ref(), startgen.as_ref()) {
                                    return self.iremove(new_inode, key, lev + 5, Some(inode), guard, startgen);
                                } else {
                                    let cnode = MainNode::CTrieNode(cnode.renewed(self, startgen.clone()));
                                    if self.gcas(inode, asdf, MainNodeStruct(cnode, Arc::new(Atomic::null()))) {
                                        return self.iremove(inode, key, lev, parent, guard, startgen);
                                    } else {
                                        return Err(())
                                    }
                                }
                            },
                            Branch::Singleton(ref snode) => {
                                // TODO fix
                                if snode.key != *key {
                                    // hash code matches, but keys aren't equal
                                    return Ok(None);
                                }

                                let new_cnode = CNode::removed(pos, flag, cnode);
                                let contracted = MainNodeStruct(self.to_contracted(new_cnode, lev), Arc::new(Atomic::null()));

                                // TODO right?
                                let result = self.gcas(inode, asdf, contracted);
                                match result {
                                    true => {
                                        if let Some(uparent) = parent {
                                            let ghj = self.gcas_read(inode, guard);
                                            match ghj.unwrap().0 {
                                                MainNode::Tomb(_) => self.clean_parent(uparent, inode, self.hash_key(key), lev - 5, startgen),
                                                _ => {},
                                            };
                                        }
                                        return Ok(Some(snode.value.clone()));
                                    },
                                    false => {
                                        return Err(());
                                    },
                                };
                            },
                        }
                    },
                    MainNode::Tomb(_) => {
                        self.clean(parent.unwrap(), lev - 5);
                        return Err(());
                    },
                    MainNode::List(ref lnode) => {
                        let new_lnode = LNode::removed(lnode, key);
                        let new_mnode = if new_lnode.nodes.len() == 1 {
                            self.entomb(new_lnode.nodes.get(0).map(|v| {
                                v.clone()
                            }).unwrap())
                        } else {
                            MainNode::List(new_lnode)
                        };

                        // TODO None? fix?
                        match self.gcas(inode, asdf, MainNodeStruct(new_mnode, Arc::new(Atomic::null()))) {
                            true => return Ok(None),
                            false => return Err(()),
                        };
                    },
                    MainNode::Failed(_) => unreachable!("dang it"),
                };
            },
            None => {},
        };

        Err(())
    }

    fn to_compressed(&self, cnode: CNode<K, V>, lev: usize) -> MainNode<K, V> {
        // TODO I think there's some unnecessary clones
        let guard = epoch::pin();

        let new_array: Vec<Arc<Branch<K, V>>> = cnode.array.iter().map(|branch| {
            match **branch {
                Branch::Indirection(ref inode) => {
                    let main = self.gcas_read(inode, &guard);
                    match main.unwrap().0 {
                        MainNode::Tomb(ref tnode) => Arc::new(Branch::Singleton(
                            tnode.clone().resurrect()
                        )),
                        _ => branch.clone(),
                    }
                },
                _ => branch.clone(),
            }
        }).collect();

        self.to_contracted(CNode {
            bmp: cnode.bmp,
            array: new_array,
        }, lev)
    }

    fn to_contracted(&self, cnode: CNode<K, V>, lev: usize) -> MainNode<K, V> {
        if lev == 0 || cnode.array.len() > 1 {
            return MainNode::CTrieNode(cnode);
        }

        match *cnode.array[0] {
            Branch::Singleton(ref snode) => self.entomb(snode.clone()),
            Branch::Indirection(_) => MainNode::CTrieNode(cnode.clone()),
        }
    }

    fn entomb(&self, snode: SNode<K, V>) -> MainNode<K, V> {
        MainNode::Tomb(
            TNode {
                singleton: snode,
            }
        )
    }

    fn flag_position(&self, hash_code: u64, lev: usize, bmp: u32) -> (u32, u32) {
        let idx = (hash_code >> lev) & 0x1f;
        let flag = 1 << idx;
        let pos = (bmp & (flag - 1)).count_ones();
        (flag, pos)
    }

    fn clean(&self, inode: &INode<K, V>, lev: usize) {
        // TODO pass in guard?
        let guard = epoch::pin();
        let m = self.gcas_read(inode, &guard);
        match m.unwrap().0 {
            MainNode::CTrieNode(ref cnode) => {
                // TODO None?
                let compressed = MainNodeStruct(self.to_compressed(cnode.clone(), lev), Arc::new(Atomic::null()));
                let _ = self.gcas(inode, m, compressed);
            },
            _ => {},
        };
    }

    fn clean_parent(&self, parent: &INode<K, V>, inode: &INode<K, V>, hashcode: u64, lev: usize, start_gen: Arc<Gen>) {
        let guard = epoch::pin();

        let parent_main = self.gcas_read(parent, &guard);
        let imain = self.gcas_read(inode, &guard);

        match parent_main.unwrap().0 {
            MainNode::CTrieNode(ref cnode) => {
                let (flag, pos) = self.flag_position(hashcode, lev, cnode.bmp);
                if cnode.bmp & flag == 0 {
                    return;
                }

                match *cnode.array[pos as usize] {
                    Branch::Indirection(ref child_inode) => {
                        if inode != child_inode {
                            return;
                        }
                    },
                    _ => return,
                };

                match imain.unwrap().0 {
                    MainNode::Tomb(ref tnode) => {
                        let new_cnode = CNode::updated(pos, cnode, Branch::Singleton(
                            tnode.clone().resurrect()
                        ));
                        let contracted = MainNodeStruct(self.to_contracted(new_cnode, lev), Arc::new(Atomic::null()));

                        if !self.gcas(parent, parent_main, contracted) && ptr_eq(self.rdcss_read_root(false, &guard).gen.as_ref(), start_gen.as_ref()) {
                            self.clean_parent(parent, inode, hashcode, lev, start_gen);
                        }
                    },
                    _ => {},
                };

            },
            _ => {},
        };
    }

    // TODO can this avoid the allocation?
    fn hash_key(&self, key: &K) -> u64 {
        let mut s = self.hash_builder.build_hasher();
        key.hash(&mut s);
        s.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::hash::{Hash, Hasher};
    use std::sync::Arc;
    use std::thread;

    #[derive(Clone, PartialEq, Eq, Debug)]
    struct SameHash {
        pub num: usize,
    }

    impl Hash for SameHash {
        fn hash<H: Hasher>(&self, state: &mut H) {
            0.hash(state);
        }
    }

    #[test]
    fn test_iter() {
        let ct = CTrie::<u64, u64>::new();
        let snap = ct.read_only_snapshot();

        assert_eq!(0, snap.iter().count());

        for i in 0..33 {
            ct.insert(i, i + 10);
        }

        assert_eq!(0, snap.iter().count());
        let snap2 = ct.read_only_snapshot();
        let mut vec2: Vec<(u64, u64)> = snap2.iter().collect();
        vec2.sort_by(|&(a, _), &(c, _)| a.cmp(&c));
        assert_eq!(33, vec2.len());
        for i in 0..33 {
            assert_eq!((i, i + 10), vec2[i as usize]);
        }

        for i in 1..32 {
            ct.remove(&i);
        }
        assert_eq!(0, snap.iter().count());
        assert_eq!(33, snap2.iter().count());
        let snap3 = ct.read_only_snapshot();
        let mut vec3: Vec<(u64, u64)> = snap3.iter().collect();
        vec3.sort_by(|&(a, _), &(c, _)| a.cmp(&c));
        assert_eq!(2, vec3.len());
        assert_eq!((0, 10), vec3[0]);
        assert_eq!((32, 42), vec3[1]);
    }

    #[test]
    fn test_snapshot() {
        let ct = CTrie::<u64, u64>::new();
        for i in 16..50 {
            ct.insert(i, i + 10);
        }

        let snap = ct.snapshot();
        for i in 16..50 {
            assert_eq!(Some(i + 10), snap.lookup(&i));
        }

        for i in 16..39 {
            ct.remove(&i);
        }
        for i in 50..90 {
            ct.insert(i, i + 10);
        }

        for i in 16..50 {
            assert_eq!(Some(i + 10), snap.lookup(&i));
        }
        for i in 50..90 {
            assert_eq!(None, snap.lookup(&i));
        }

        for i in 0..16 {
            snap.insert(i, i + 10);
        }
        for i in 35..50 {
            snap.remove(&i);
        }

        for i in 0..35 {
            assert_eq!(Some(i + 10), snap.lookup(&i));
        }
        for i in 35..90 {
            assert_eq!(None, snap.lookup(&i));
        }
        for i in 0 ..39 {
            assert_eq!(None, ct.lookup(&i));
        }
        for i in 39..90 {
            assert_eq!(Some(i + 10), ct.lookup(&i));
        }
    }

    #[test]
    fn test_33_inserts() {
        let ct = CTrie::<u64, u64>::new();
        for idx in 0..33 {
            ct.insert(idx, idx + 100);
        }

        for idx in 0..33 {
            assert_eq!(Some(idx + 100), ct.lookup(&idx));
        }

        for idx in 33..133 {
            assert_eq!(None, ct.lookup(&idx));
        }
    }

    #[test]
    fn test_remove() {
        let ct = CTrie::<u64, u64>::new();
        for idx in 0..33 {
            ct.insert(idx, idx + 100);
        }

        for idx in 0..33 {
            ct.remove(&idx);

            for inner_idx in 0..idx+1 {
                assert_eq!(None, ct.lookup(&inner_idx));
            }

            for inner_idx in idx+1..33 {
                assert_eq!(Some(inner_idx + 100), ct.lookup(&inner_idx));
            }
        }
    }

    #[test]
    fn test_hash_collisions() {
        let ct = CTrie::<SameHash, usize>::new();

        for idx in 0..4 {
            ct.insert(SameHash{
                num: idx,
            }, idx + 100);
        }

        for idx in 0..4 {
            assert_eq!(Some(idx + 100), ct.lookup(&SameHash{
                num: idx,
            }));
        }
    }

    // #[test]
    // fn test_concurrency() {
    //     let ct = Arc::new(CTrie::<u64, u64>::new());
    //
    //     let mut guards = Vec::<thread::JoinHandle<()>>::new();
    //     for idx in 0..100 {
    //         let c = ct.clone();
    //         guards.push(thread::spawn(move || {
    //             for num in 0..10000 {
    //                 let num_to_insert = (100 * num) + idx;
    //                 c.insert(num_to_insert, num_to_insert + 10000);
    //             }
    //         }));
    //     }
    //
    //     for guard in guards {
    //         guard.join().unwrap();
    //     }
    //
    //     for idx in 0..1000000 {
    //         assert_eq!(Some(idx + 10000), ct.lookup(&idx));
    //     }
    // }
}
