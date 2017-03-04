use std::hash::Hash;
use std::hash::BuildHasher;

use crossbeam::mem::epoch;

use ctrie::CTrie;
use ctrie::CasHelper;
use node::Branch;
use node::MainNode;
use node::MainNodeStruct;

pub struct CTrieIter<'a, K, V, H> where K: 'a + Hash + Eq + Clone, V: 'a + Clone, H: 'a + BuildHasher + Clone {
    // TODO
    ct: &'a CTrie<K, V, H>,
    stack: Vec<State<K, V>>,
    // guard: Guard,

}

// TODO remove excessive clones everywhere
// struct State<'a, K: 'a + Clone, V: 'a + Clone> {
#[derive(Clone)]
struct State<K: Clone, V: Clone> {
    // node: &'a MainNodeStruct<K, V>,
    // TODO
    node: MainNodeStruct<K, V>,
    idx: usize,
}

impl<'a, K: Hash + Eq + Clone, V: Clone, H: BuildHasher + Clone> CTrieIter<'a, K, V, H> {
    pub fn new(ct: &'a CTrie<K, V, H>) -> CTrieIter<'a, K, V, H> {
        let guard = epoch::pin();
        let mut c = CTrieIter {
            ct: ct,
            // 13 is the deepest this can go
            stack: Vec::with_capacity(13),
        };
        let inode = CasHelper::rdcss_read_root(ct, false, &guard);

        let mn = CasHelper::gcas_read(ct, *inode, &guard);
        c.stack.push(State {
            node: (**mn.unwrap()).clone(),
            idx: 0,
        });

        c
    }
}

impl<'a, K: Hash + Eq + Clone, V: Clone, H: BuildHasher + Clone> Iterator for CTrieIter<'a, K, V, H> {
    type Item = (K, V);

    fn next(&mut self) -> Option<(K, V)> {
        let len = self.stack.len();
        if len == 0 {
            return None;
        }

        // let mut last = self.stack.split_off(len - 1)[0];
        let mut last = self.stack.remove(len - 1);
        match last.node.0 {
            MainNode::CTrieNode(ref cnode) => {
                if last.idx >= cnode.array.len() {
                    return self.next();
                }

                match *cnode.array[last.idx] {
                    Branch::Singleton(ref snode) => {
                        last.idx += 1;
                        let k = snode.key.clone();
                        let v = snode.value.clone();
                        // if last.idx < len {
                            self.stack.push(last.clone());
                        // }
                        return Some((k, v));
                    },
                    Branch::Indirection(ref inode) => {
                        // Need to push current with index plus one
                        last.idx += 1;
                        // if last.idx < len {
                            self.stack.push(last.clone());
                        // }
                        // push new node with node=gcas_read(inode), idx=0
                        let guard = epoch::pin();
                        let shared = CasHelper::gcas_read(self.ct, inode, &guard);
                        self.stack.push(State {
                            node: (**shared.unwrap()).clone(),
                            idx: 0,
                        });
                        // recurse
                        return self.next();
                    },
                };
            },
            MainNode::List(ref lnode) => {
                // TODO efficiency
                // Need to push current with index plus one
                let len = lnode.nodes.len();
                let snode = lnode.nodes.get(last.idx);
                last.idx += 1;
                if last.idx < len {
                    self.stack.push(last.clone());
                }
                return snode.map(|node| (node.key.clone(), node.value.clone()));
            },
            MainNode::Tomb(_) => {}, // ignore
            MainNode::Failed(_) => unreachable!("shit"),
        };

        None
    }
}
