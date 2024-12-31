package INF102.lab3.numberGuesser;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

public class NumberGuesserTest {
    
    public static final int LOWERBOUND = 0;
	public static final int UPPERBOUND = 10000;
    public static final int N_GAMES = 1000;

    static IGuesser random;
    static IGuesser genius;

    @BeforeAll
    public static void setup() {
        random = new RandomGuesser();
        genius = new MyGeniusGuesser();
    }

    @Test
    public void correctNumberGuessTest() {
        RandomNumber number = new RandomNumber(LOWERBOUND, UPPERBOUND);
        int randomAnswer = random.findNumber(number);
        int geniusAnswer = genius.findNumber(number);
        assertEquals(randomAnswer, geniusAnswer);
    }

    @Test
    public void betterThanRandomTest() {
        int averageRandomGuess = 0;
        int averageGeniusGuess = 0;
        for (int i = 0; i < N_GAMES; i++) {
            RandomNumber number = new RandomNumber(LOWERBOUND, UPPERBOUND);

            random.findNumber(new RandomNumber(number));
            int randomCount = number.getGuessCount();

            genius.findNumber(new RandomNumber(number));
            int geniusCount = number.getGuessCount();

            averageRandomGuess += randomCount;
            averageGeniusGuess += geniusCount;
        }
        averageRandomGuess /= N_GAMES;
        averageGeniusGuess /= N_GAMES;

        if ((averageRandomGuess/800) < averageGeniusGuess) {
            fail("Your guesser was not smart enough");
        }

    }
}
