package INF102.lab3.peakElement;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

import org.javatuples.Pair;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

public class PeakTest {
    
    static List<Pair<Integer, List<Integer>>> testCases;

    static IPeak iterative = new PeakIterative();
    static IPeak recursive = new PeakRecursive();

    @BeforeAll
    public static void setup() {
        String filePath = "src/test/java/INF102/lab3/peakElement/PeakExamples.txt";
        try {
            testCases = getTestCases(filePath);  
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            System.exit(1);
        }   
    }

    
    public static List<Pair<Integer, List<Integer>>> getTestCases(String filePath) throws FileNotFoundException {
        File file = new File(filePath);
        Scanner in = new Scanner(file);

        List<Pair<Integer, List<Integer>>> pairs = new ArrayList<>();
        while (in.hasNextLine()) {
            String line1 = in.nextLine();
            String line2 = in.nextLine();

            int target = Integer.parseInt(line1);
            List<Integer> numbers = new ArrayList<>();
            for (String number : Arrays.asList(line2.split(" "))) {
                numbers.add(Integer.parseInt(number));
            }
            Pair<Integer, List<Integer>> pair = new Pair<>(target, numbers);
            pairs.add(pair);
        }
        in.close();
        return pairs;
    }

    /* ############### FUNCTIONALITY TESTS ############### */
    @Test
    public void recursivePeakTest() {
        for (Pair<Integer, List<Integer>> pair : testCases) {
            int target = pair.getValue0();
            List<Integer> numbers = pair.getValue1();
            int peakRecursive = recursive.peakElement(numbers);
            assertEquals(target, peakRecursive);
        }
    }

    /* ############### SYNTAX TESTS ############### */
    @Test
    public void doesNotUseLoops() {
        String filePath = "src/main/java/INF102/lab3/peakElement/PeakRecursive.java";
        try {
            String fileContent = readFile(filePath);
            if (fileContent.contains("for") || fileContent.contains("while"))
                fail("You cannot use for or while loop for this task.");
        } catch (IOException e) {
            e.printStackTrace();
            fail("File was not found.");
        }
    }

    public String readFile(String filePath) throws IOException {
        byte[] encoded = Files.readAllBytes(Paths.get(filePath));
        return new String(encoded, StandardCharsets.US_ASCII);
    }
}
