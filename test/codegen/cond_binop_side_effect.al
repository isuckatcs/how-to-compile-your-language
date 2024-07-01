// RUN: compiler %s -o cond_binop_side_effect && ./cond_binop_side_effect | grep -Plzx '1.000000\n2.000000\n3.000000\n4.000000\n5.000000\n7.000000\n10.000000\n13.000000\n14.000000\n15.000000\n16.000000\n'
fn true(x: number): number {
    print(x);
    return 1.0;
}

fn false(x: number): number {
    print(x);
    return 0.0;
}

fn main(): void {
    false(1.0) || true(2.0) && false(3.0);
    
    false(4.0) || true(5.0) || true(6.0);

    false(7.0) && false(8.0) && true(9.0);
    
    true(10.0) || true(11.0) || true(12.0);

    false(13.0) || true(14.0) && false(15.0) || true(16.0);

}
