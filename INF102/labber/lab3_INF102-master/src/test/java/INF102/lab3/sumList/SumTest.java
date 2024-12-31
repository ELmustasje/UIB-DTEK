package INF102.lab3.sumList;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import org.junit.jupiter.api.Test;

import INF102.lab3.utils.Range;

public class SumTest {

    static ISum iterative = new SumIterative();
    static ISum recursive = new SumRecursive();

    static final long UPPERBOUND = 1000;
    static final List<Long> numbers = Range.range(UPPERBOUND);

    /* ############### FUNCTIONALITY TESTS ############### */
    @Test
    public void recursiveSumTest() {
        for (long i = 0; i < 10000; i++) {
            List<Long> numberRange = Range.range(i);
            Long iterativeSum = iterative.sum(numberRange);
            long recursiveSum = recursive.sum(numberRange);
            assertEquals(iterativeSum, recursiveSum);
        }
    }

      /* ############### RUNTIME TESTS ############### */
    @Test
	public void faster() {
        long benchMarkTime = 0;
		long myTime = 0;
        for (int i = 0; i < 10; i++) {
            benchMarkTime += timeAlgorithm(iterative);
		    myTime += timeAlgorithm(recursive);
        }
		
		if ((benchMarkTime*2) < myTime) {
			fail("Your algorithm was too slow. Maybe there is an optimization you could do?.");
		}
	}

	public long timeAlgorithm(ISum algorithm) {
		long startTime = System.nanoTime();
		algorithm.sum(numbers);
		algorithm.sum(numbers);
		long endTime = System.nanoTime();
		long timeElapsed = (endTime - startTime);
		return timeElapsed;
	}

    /* ############### SYNTAX TESTS ############### */
    @Test
    public void doesNotUseLoops() {
        String filePath = "src/main/java/INF102/lab3/sumList/SumRecursive.java";
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
