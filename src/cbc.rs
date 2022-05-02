
use feanor_la::algebra::fq::fq_small::*;
use feanor_la::la::mat::*;
use feanor_la::ring::*;

type F2El = <F2Type as Ring>::El;

pub fn enc<F, V>(mut block_enc: F, iv: Vector<V, F2El>, message: Vec<F2El>) -> Vec<F2El>
    where F: FnMut(Vector<V, F2El>) -> Vector<V, F2El>, V: VectorViewMut<F2El>
{
    let mut result: Vec<F2El> = Vec::new();
    let n = iv.len();
    let mut c = iv;
    let mut message = message.into_iter().peekable();
    result.extend(c.iter());
    while message.peek().is_some() {
        for (i, bit) in message.by_ref().take(n).enumerate() {
            *c.at_mut(i) += bit;
        }
        c = block_enc(c);
        result.extend(c.iter());
    }
    return result;
}

pub fn dec<F>(mut block_dec: F, n: usize, ciphertext: Vec<F2El>) -> Vec<F2El>
    where F: FnMut(Vector<VectorOwned<F2El>, F2El>) -> Vector<VectorOwned<F2El>, F2El>
{
    let mut result: Vec<F2El> = Vec::new();
    let mut ciphertext = ciphertext.into_iter().peekable();
    let mut c_last = Vector::from_fn(n, |_| ciphertext.next().unwrap());
    while ciphertext.peek().is_some() {
        let c = Vector::from_fn(n, |_| ciphertext.next().unwrap());
        result.extend((block_dec(c.clone()) + c_last).iter());
        c_last = c;
    }
    return result;
}

#[cfg(test)]
use feanor_la::embedding::*;

#[test]
fn test_enc_dec() {
    let i = z_hom(&F2);
    let key = Vector::from_array([i(0), i(1), i(0), i(1)]);
    let block_enc = |x: Vector<VectorOwned<F2El>, F2El>| x + key.as_ref();
    let block_dec = |x: Vector<VectorOwned<F2El>, F2El>| x + key.as_ref();
    let iv = Vector::from_array([i(1), i(1), i(1), i(0)]);
    let message = vec![i(0), i(0), i(0), i(0), i(1), i(1), i(1), i(1)];
    let ciphertext = enc(block_enc, iv, message.clone());
    let decryption = dec(block_dec, 4, ciphertext);
    assert_eq!(message, decryption);
}

#[cfg(test)]
use super::lfsr::*;

#[test]
fn test_attack() {

    fn eve<F>(ciphertext: Vec<F2El>, mut encryption_oracle: F) -> Vec<F2El>
        where F: FnMut(Vec<F2El>) -> Vec<F2El>
    {
        let i = z_hom(&F2);
        // This is a chosen-plaintext attack:
        // we assume we know that message is one of
        let m1 = Vector::from_array([i(0), i(0), i(0), i(0), i(1), i(1), i(1), i(1)]);
        let m2 = Vector::from_array([i(0), i(0), i(0), i(0), i(0), i(0), i(0), i(0)]);
        // we may not call encryption_oracle on m1 or m2, but on all other messages

        let prev_iv = Vector::new(Vec::from(&ciphertext[..4]));
        let iv_gen = attack_lfsr(F2, &ciphertext[..4]);
        let mut iv_stream = iv_gen.value_stream(Vec::from(&ciphertext[2..4]));
        let iv = Vector::new(iv_stream.by_ref().take(4).collect::<Vec<_>>());
        
        let mut m = m1.clone();
        *&mut m.subvector_mut(0..4) += iv;
        *&mut m.subvector_mut(0..4) += prev_iv;
        let c = encryption_oracle(m.raw_data().to_vec());

        if &c[4..] == &ciphertext[4..] {
            return m1.raw_data().to_vec();
        } else {
            return m2.raw_data().to_vec();
        }
    }
        
    let i = z_hom(&F2);
    let key = Vector::from_array([i(0), i(1), i(0), i(1)]);
    let block_enc = |x: Vector<VectorOwned<F2El>, F2El>| x + key.as_ref();
    let message = vec![i(0), i(0), i(0), i(0), i(0), i(0), i(0), i(0)];
    let iv_gen = LFSR::new(F2, Vector::from_array([i(1), i(1)]));
    let mut iv_stream = iv_gen.value_stream(vec![i(1), i(0)]);
    let mut enc = |m: Vec<F2El>| enc(block_enc, Vector::new(iv_stream.by_ref().take(4).collect()), m);

    let ciphertext = enc(message.clone());
    let eves_guess = eve(ciphertext, &mut enc);
    assert_eq!(message, eves_guess);
}