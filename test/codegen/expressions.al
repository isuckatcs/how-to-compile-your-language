// RUN: compiler %s -o expressions && ./expressions | grep -Plzx '1.000000\n-1.000000\n-9.000000\n'
fn foo(x: number): number {
    if x < (0.0 - 5.0) {
        return (0.0 - 1.0);
    } else if x > 5.0 {
        return 1.0;
    }

    let x: number = 2.0 + 3.0;
    
    var y: number = !x;
    
    y = 3.0 * !y / (1.5 + 1.5);

    return y - 10.0;
}

fn main(): void {
    print(foo(6.0));
    print(foo(0.0 - 6.0));
    print(foo(1.0));
}
