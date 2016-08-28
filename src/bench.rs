

#[cfg(test)]
mod tests {
    use super::*;
    use test::{self, Bencher};
    use ctrie::CTrie;
    use std::collections::HashMap;

    pub fn add_two(a: u32) -> u32 {
        a + 2
    }

    #[test]
    fn it_works() {
        assert_eq!(4, add_two(2));
    }

    #[bench]
    fn bench_basic_hm_insert(b: &mut Bencher) {
        let mut hm = HashMap::<u64, u64>::new();
        let mut num = 1;

        b.iter(move || {
            hm.insert(num, num + 10);
            num += 1;
        })
    }

    #[bench]
    fn bench_basic_ct_insert(b: &mut Bencher) {
        let ct = CTrie::<u64, u64>::new();
        let mut num = 1;

        b.iter(move || {
            ct.insert(num, num + 10);
            num += 1;
        })
    }
}
