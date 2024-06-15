fn ret(): void {

}

fn printParam(n: number): void {
    if (ret()) {
        print(1.0);
    } else if (n == 3.0) {
        print(2.0);
    }

    print(30.0);
}

fn main(): void {
    printParam(0.0);
}
