// RUN: compiler %s -llvm-dump 2>&1 | filecheck %s --implicit-check-not error
// CHECK: {{.*}}
fn foo(x: number): void {
    let x: number = 1.0;

    if 0.0 {
        let x: number = 2.0;

        if 0.0 {
            let x: number = 3.0;
        }
    } else {
        let x: number = 4.0;
    }

    while 0.0 {
        let x: number = 5.0;

        if 0.0 {
            let x: number = 6.0;

            if 0.0 {
                let x: number = 7.0;
            }
        } else {
            let x: number = 8.0;
        }
    }
}

fn main(): void {
    foo(0.0);
}
