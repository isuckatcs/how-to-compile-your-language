fn divides(n: number, divisor: number): number {
    var i: number = 1.0;
    while !(i > n) {
        let d: number = divisor * i;

        if d == n {
            return 1.0;
        }

        i = i + 1.0;
    }

    return 0.0;
}

fn isPrime(x: number): number {
    if x == 0.0 || x == 1.0 {
        return 0.0;
    }

    var i: number = 2.0;
    while !(i > x / 2.0) {
        if divides(x, i) {
            return 0.0;
        }
        
        i = i + 1.0;
    }

    return 1.0;
}

fn main(): void {
    var n: number = 1.0;
    var cnt: number = 0.0;

    while cnt < 10.0 {
        if isPrime(n) {
            print(n);
            cnt = cnt + 1.0;
        }

        n = n + 1.0;
    }
}
