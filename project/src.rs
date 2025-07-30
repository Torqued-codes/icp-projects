use ic_cdk::export::candid::{CandidType, Deserialize};
use ic_cdk_macros::*;
use std::cell::RefCell;

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct RandomResult {
    pub user_input: i32,
    pub random_number: i32,
    pub difference: i32,
}

// Linear congruential generator for pseudo-random numbers
thread_local! {
    static RNG_STATE: RefCell<u64> = RefCell::new(1);
}

fn generate_random_number() -> u32 {
    RNG_STATE.with(|state| {
        let mut rng = state.borrow_mut();
        // Use current time as seed for better randomness
        let time_seed = ic_cdk::api::time();
        *rng = rng.wrapping_mul(1664525).wrapping_add(1013904223).wrapping_add(time_seed);
        (*rng >> 16) as u32
    })
}

#[update]
fn calculate_difference(user_input: i32) -> RandomResult {
    // Generate random number between 1 and 100
    let random_u32 = generate_random_number();
    let random_number = (random_u32 % 100) as i32 + 1;
    
    // Calculate absolute difference
    let difference = (user_input - random_number).abs();
    
    RandomResult {
        user_input,
        random_number,
        difference,
    }
}

#[query]
fn get_info() -> String {
    "Random Number Difference Calculator - Built on Internet Computer".to_string()
}

// Export Candid interface
ic_cdk::export_candid!();