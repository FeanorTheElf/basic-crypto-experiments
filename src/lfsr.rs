
use feanor_la::algebra::fq::fq_small::*;
use feanor_la::la::mat::*;
use feanor_la::ring::*;
use feanor_la::embedding::*;

use std::ops::Index;

pub struct LFSR<R = F2Type, V = VectorOwned<<R as Ring>::El>>
    where R: Ring, V: VectorView<<R as Ring>::El>
{
    ring: R,
    coefficients: Vector<V, <R as Ring>::El>
}

pub struct LFSRState<R = F2Type>
    where R: Ring
{
    data: Box<[<R as Ring>::El]>,
    first_el: usize
}

impl<R: Ring> LFSRState<R> {

    pub fn push(&mut self, el: R::El) {
        if self.first_el == 0 {
            self.first_el = self.len() - 1;
        } else {
            self.first_el -= 1;
        }
        self.data[self.first_el] = el;
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl<R: Ring> Index<usize> for LFSRState<R> {

    type Output = R::El;

    fn index(&self, index: usize) -> &Self::Output {
        if self.first_el + index >= self.data.len() {
            &self.data[self.first_el + index - self.len()]
        } else {
            &self.data[self.first_el + index]
        }
    }
}

impl<R, V> LFSR<R, V> 
    where R: Ring, V: VectorView<<R as Ring>::El>
{
    pub fn new(ring: R, coefficients: Vector<V, <R as Ring>::El>) -> Self {
        LFSR {
            ring, coefficients
        }
    }

    pub fn value_stream<'a>(&'a self, seed: Vec<<R as Ring>::El>) -> impl 'a + Iterator<Item = <R as Ring>::El> {
        assert_eq!(self.coefficients.len(), seed.len());
        let state = LFSRState {
            data: seed.into_boxed_slice(),
            first_el: 0
        };
        std::iter::repeat(()).scan(state, |state: &mut LFSRState<R>, ()| {
            let result = self.ring.sum(
                (0..self.coefficients.len()).map(|i| self.ring.mul_ref(&state[i], &self.coefficients[i]))
            );
            state.push(result.clone());
            return Some(result);
        })
    }
}

pub fn berlekamp_massey<F>(field: F, sequence: &[F::El]) -> Vector<VectorOwned<F::El>, F::El> 
    where F: Ring
{
    assert!(field.is_field().can_use());
    let i = z_hom(&field);
    let mut current = Vector::from_array([i(1)]);
    let mut last_discrepancy_index: i64 = -1;
    let mut last_discrepancy = i(1);
    let mut last_discrepancy_vector = current.clone();
    for n in 0..sequence.len() {

        let discrepancy = field.sum(
            (0..current.len()).map(|j| field.mul_ref(&current[j], &sequence[n - j]))
        );

        if field.is_zero(&discrepancy) {
            // everything is fine
        } else {
            let tmp = current.clone();

            let factor = field.neg(field.div(discrepancy.clone(), &last_discrepancy));
            let shift = (n as i64 - last_discrepancy_index) as usize;
            
            current = Vector::from_fn(shift + last_discrepancy_vector.len(),
                |j| if j >= current.len() && j < shift {
                    field.zero()
                } else if j >= current.len() {
                    field.mul_ref(&last_discrepancy_vector[j - shift], &factor)
                } else if j < shift {
                    current[j].clone()
                } else {
                    field.add_ref(field.mul_ref(&last_discrepancy_vector[j - shift], &factor), &current[j].clone())
                }
            );

            last_discrepancy_index = n as i64;
            last_discrepancy = discrepancy;
            last_discrepancy_vector = tmp;
        }
    }
    return current;
}

#[cfg(test)]
use feanor_la::primitive::*;

#[test]
fn test_lfsr_value_stream() {
    let lfsr = LFSR::new(
        i64::RING,
        Vector::from_array([1, 1])
    );
    let stream = lfsr.value_stream(vec![1, 0]);
    assert_eq!(vec![1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144], stream.take(11).collect::<Vec<_>>());
}

#[test]
fn test_binary_lfsr_value_stream() {
    let i = z_hom(&F2);
    let lfsr = LFSR::new(
        F2Type::RING,
        Vector::from_array([i(1), i(1)])
    );
    let stream = lfsr.value_stream(vec![i(1), i(0)]);
    assert_eq!(vec![i(1), i(0), i(1), i(1), i(0)], stream.take(5).collect::<Vec<_>>());
}

#[test]
fn test_berlekamp_massey() {
    let i = z_hom(&F2);
    let lfsr = LFSR::new(
        F2Type::RING,
        Vector::from_array([i(0), i(1), i(1)])
    );
    let sequence = lfsr.value_stream(vec![i(1), i(0), i(1)]).take(20).collect::<Vec<_>>();
    assert!(Vector::from_array([i(1), i(0), i(1), i(1)]).eq(berlekamp_massey(F2, &sequence[..]), &F2));
}