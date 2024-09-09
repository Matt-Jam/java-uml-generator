// Define the Car class
public class Car {

    // Attributes (fields)
    private String make;
    private String model;
    private int year;
    private double mileage;
    private boolean isEngineOn;

    // Constructor to initialize the car object
    public Car(String make, String model, int year, double mileage){
        this.make = make;
        this.model = model;
        this.year = year;
        this.mileage = mileage;
        this.isEngineOn = false; // Engine is off by default
    }

    // Method to start the car
    public void startEngine() {
        if (!isEngineOn) {
            isEngineOn = true;
            System.out.println("The engine has started.");
        } else {
            System.out.println("The engine is already on.");
        }
    }

    // Method to stop the car
    public void stopEngine() {
        if (isEngineOn) {
            isEngineOn = false;
            System.out.println("The engine has stopped.");
        } else {
            System.out.println("The engine is already off.");
        }
    }

    // Method to drive the car, increasing the mileage
    public void drive(double distance) {
        if (isEngineOn) {
            mileage += distance;
            System.out.println("You have driven " + distance + " miles.");
        } else {
            System.out.println("You need to start the engine first.");
        }
    }

    // Getter method for car mileage
    public double getMileage() {
        return mileage;
    }

    // Getter method for car make
    public String getMake() {
        return make;
    }

    // Getter method for car model
    public String getModel() {
        return model;
    }

    // Getter method for car year
    public int getYear() {
        return year;
    }

    // Method to display car details
    public void displayCarInfo() {
        System.out.println("Car Make: " + make);
        System.out.println("Car Model: " + model);
        System.out.println("Car Year: " + year);
        System.out.println("Car Mileage: " + mileage);
        System.out.println("Engine On: " + (isEngineOn ? "Yes" : "No"));
    }

    // Main method for testing
    public static void main(String[] args) {
        // Create a car object
        Car myCar = new Car("Toyota", "Corolla", 2020, 15000);

        // Display car info
        myCar.displayCarInfo();

        // Start the engine and drive
        myCar.startEngine();
        myCar.drive(120.5);

        // Stop the engine and display updated info
        myCar.stopEngine();
        myCar.displayCarInfo();
    }
}
