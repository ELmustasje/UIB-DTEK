package INF101.lab1.rockPaperScissors;

import java.util.List;
import java.util.Arrays;
import java.util.Random;
import java.util.Scanner;


public class RockPaperScissors {
	
    public static void main(String[] args) {
    	/* 	
    	 * The code here does two things:
    	 * It first creates a new RockPaperScissors -object with the
    	 * code `new RockPaperScissors()`. Then it calls the `run()`
    	 * method on the newly created object.
         */
        new RockPaperScissors().run();
    }
    
    
    Scanner sc = new Scanner(System.in);
    int roundCounter = 1;
    int humanScore = 0;
    int computerScore = 0;
    String keepPlaying;
    List<String> rpsChoices = Arrays.asList("rock", "paper", "scissors");
    
    public void run() {
        // TODO: Implement me :)
        Random rand = new Random();
        do {
            System.out.println("Let's play round " + roundCounter);
            String valg = readInput("Your choice (Rock/Paper/Scissors)?");
            String pcValg = getRandomRps();
            if(valg.equals(pcValg)){
                System.out.println("Human chose "+valg +", computer chose "+pcValg+". It's a tie!");
            } else{
                if(spillerVinner(valg,pcValg)){
                    humanScore++;
                    System.out.println("Human chose "+valg +", computer chose "+pcValg+". Human wins!");
                }else {
                    computerScore++;
                    System.out.println("Human chose "+valg +", computer chose "+pcValg+". Computer wins!");
                }
            }
            System.out.println("Score: human "+humanScore+", computer "+computerScore);
            keepPlaying = readInput("Do you wish to continue playing? (y/n)?");
            roundCounter++;

        } while(keepPlaying.equals("y"));
        System.out.println("Bye bye :)");
    }

    /**
     * Reads input from console with given prompt
     * @param prompt
     * @return string input answer from user
     */
    static Boolean spillerVinner(String valg,String pcValg){
        if (valg.equals("rock")) {
            return pcValg.equals("scissors");
        } else if (valg.equals("paper")) {
            return pcValg.equals("rock");
        } else {
            return pcValg.equals("paper");
        }
    }
    public String getRandomRps(){
        Random rand = new Random();
        List<String> rpsChoices = Arrays.asList("rock", "paper", "scissors");
        return rpsChoices.get(rand.nextInt(0,3));
    }
    public String readInput(String prompt) {
        System.out.println(prompt);
        String userInput = sc.next();
        return userInput;
    }

}