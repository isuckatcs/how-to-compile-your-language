// RUN: compiler %s -o print && ./print | grep -Plzx '0\n1\n1.2345\n1.0002345\n12345.6789\n'
fn main(): void {
    print(0.0);
    print(01.0);
    print(1.234500000);
    print(1.000234500000);
    print(12345.6789);
}
