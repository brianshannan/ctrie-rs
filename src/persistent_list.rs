use std::fmt;
use std::iter::FromIterator;
use std::sync::Arc;

#[derive(Clone)]
pub enum PersistentList<T: Clone> {
    Nil,
    Cons(T, Arc<PersistentList<T>>),
}

pub struct IntoIter<T: Clone>(PersistentList<T>);

impl<T: Clone + fmt::Debug> fmt::Debug for PersistentList<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl<T: Clone> FromIterator<T> for PersistentList<T> {
    fn from_iter<U: IntoIterator<Item = T>>(iter: U) -> PersistentList<T> {
        let mut list = PersistentList::new();
        // reverse to achieve O(1) time per individual insert
        // for O(n) total time instead of O(n^2)
        for value in iter.into_iter().collect::<Vec<T>>().into_iter().rev() {
            list = list.prepend(value);
        }
        list
    }
}

impl<T: Clone> PersistentList<T> {
    pub fn new() -> PersistentList<T> {
        PersistentList::Nil
    }

    pub fn head(&self) -> Option<T> {
        match *self {
            PersistentList::Nil => None,
            PersistentList::Cons(ref head, _) => Some(head.clone()),
        }
    }

    pub fn tail(&self) -> PersistentList<T> {
        match *self {
            PersistentList::Nil => PersistentList::Nil,
            PersistentList::Cons(_, ref next) => (**next).clone(),
        }
    }

    pub fn len(&self) -> usize {
        match *self {
            PersistentList::Nil => 0,
            PersistentList::Cons(_, ref next) => 1 + (**next).len(),
        }
    }

    pub fn prepend(&self, elem: T) -> PersistentList<T> {
        PersistentList::Cons(elem, Arc::new(self.clone()))
    }

    pub fn insert(&self, pos: usize, elem: T) -> PersistentList<T> {
        if pos == 0 {
            return self.prepend(elem);
        }

        let new_node = self.tail().insert(pos - 1, elem);
        match *self {
            PersistentList::Nil => new_node,
            PersistentList::Cons(ref head, _) =>
                PersistentList::Cons(head.clone(), Arc::new(new_node)),
        }
    }

    pub fn get(&self, pos: usize) -> Option<T> {
        if pos > 0 {
            return self.tail().get(pos - 1);
        }

        match *self {
            PersistentList::Nil => None,
            PersistentList::Cons(ref head, _) => Some(head.clone()),
        }
    }

    pub fn remove(&self, pos: usize) -> PersistentList<T> {
        if pos == 0 {
            return self.tail();
        }

        match *self {
            PersistentList::Nil => PersistentList::Nil,
            PersistentList::Cons(ref head, _) => {
                let new_node = self.tail().remove(pos - 1);
                PersistentList::Cons(head.clone(), Arc::new(new_node))
            }
        }
    }

    pub fn into_iter(self) -> IntoIter<T> {
        IntoIter(self)
    }

    pub fn iter(&self) -> IntoIter<T> {
        self.clone().into_iter()
    }
}

impl<T: Clone> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        let head = self.0.head();
        self.0 = self.0.tail();
        head
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.0.len();
        (len, Some(len))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_list(list: &PersistentList<u32>) {
        assert_eq!(4, list.len());
        assert_eq!(Some(13), list.head());
        let list = list.tail();
        assert_eq!(3, list.len());
        assert_eq!(Some(9), list.head());
        let list = list.tail();
        assert_eq!(2, list.len());
        assert_eq!(Some(6), list.head());
        let list = list.tail();
        assert_eq!(1, list.len());
        assert_eq!(Some(4), list.head());
        let list = list.tail();
        assert_eq!(0, list.len());
        assert_eq!(None, list.head());
    }

    #[test]
    pub fn test_prepend() {
        let mut list = PersistentList::<u32>::new();
        list = list.prepend(4);
        list = list.prepend(6);
        list = list.prepend(9);
        list = list.prepend(13);

        assert_list(&list);
    }

    #[test]
    pub fn test_insert() {
        let mut list = PersistentList::<u32>::new();
        list = list.prepend(6);
        list = list.insert(0, 13);
        list = list.insert(1, 9);
        list = list.insert(3, 4);

        assert_list(&list);
    }

    #[test]
    pub fn test_remove() {
        let mut list = PersistentList::<u32>::new();
        list = list.prepend(4);
        list = list.prepend(6);
        list = list.prepend(9);
        list = list.prepend(13);

        list = list.remove(1);
        assert_eq!(3, list.len());
        assert_eq!(Some(13), list.get(0));
        assert_eq!(Some(6), list.get(1));
        assert_eq!(Some(4), list.get(2));

        list = list.remove(2);
        assert_eq!(2, list.len());
        assert_eq!(Some(13), list.get(0));
        assert_eq!(Some(6), list.get(1));

        list = list.remove(0);
        assert_eq!(1, list.len());
        assert_eq!(Some(6), list.get(0));

        list = list.remove(0);
        assert_eq!(0, list.len());
        assert_eq!(None, list.get(0));
    }
}
