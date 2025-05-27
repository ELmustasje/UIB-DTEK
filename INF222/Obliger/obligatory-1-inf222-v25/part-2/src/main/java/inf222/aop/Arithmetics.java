package inf222.aop;

public class Arithmetics {

    private double a_GBP;
    private double b_USD;
    private double c_NOK;
    private double d_JPY;
    private double e_DKK;

    // Constructor: initialize the fields with sample values.
    public Arithmetics() {
        a_GBP = 5.0;
        b_USD = 3.0;
        c_NOK = 100.0;
        d_JPY = 200.0;
        e_DKK = 50.0;
    }

    public void runExample() {
        System.out.println("Initial Values:");
        printValues();

        a_GBP += b_USD * 2;
        c_NOK -= 10;
        d_JPY *= 1.05;
        e_DKK /= 2;
        b_USD += 1.5;
        // b_USD = -20; // illegal

        System.out.println("\nAfter First Operations:");
        printValues();

        double sum = a_GBP + b_USD + c_NOK + d_JPY + e_DKK;
        double average = sum / 5;
        double product = a_GBP * b_USD * c_NOK * d_JPY * e_DKK;
        double difference = c_NOK - a_GBP;

        System.out.println("\nComputed Values:");
        System.out.println("Sum of all currencies: " + sum);
        System.out.println("Average value: " + average);
        System.out.println("Product of all currencies: " + product);
        System.out.println("Difference (NOK - GBP): " + difference);
    }

    private void printValues() {
        System.out.println("a_GBP: " + a_GBP);
        System.out.println("b_USD: " + b_USD);
        System.out.println("c_NOK: " + c_NOK);
        System.out.println("d_JPY: " + d_JPY);
        System.out.println("e_DKK: " + e_DKK);
    }

    public static void main(String[] args) {
        (new Arithmetics()).runExample();
    }

    // Expected output:

    // Initial Values:
    // a_GBP: 70.0
    // b_USD: 33.0
    // c_NOK: 100.0
    // d_JPY: 14.000000000000002
    // e_DKK: 75.0

    // After First Operations:
    // a_GBP: 136.0
    // b_USD: 34.5
    // c_NOK: 90.0
    // d_JPY: 14.700000000000003
    // e_DKK: 37.5

    // Computed Values:
    // Sum of all currencies: 312.7
    // Average value: 62.54
    // Product of all currencies: 2.3278185000000003E8
    // Difference (NOK - GBP): -46.0
}
