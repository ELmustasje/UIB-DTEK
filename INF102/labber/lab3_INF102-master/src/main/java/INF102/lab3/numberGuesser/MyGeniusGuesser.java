package INF102.lab3.numberGuesser;

import java.util.Random;

public class MyGeniusGuesser implements IGuesser {

  private Random rand = new Random();
  @Override
  public int findNumber(RandomNumber number) {
    return binarySearch(number,number.getLowerbound(),number.getUpperbound());
  }
  public int binarySearch(RandomNumber number, int low, int high) {
    int guess = low + (high - low) / 2;
    int result = number.guess(guess);

    if(result == 0){
      return guess;
    } else if(result == 1){
      return binarySearch(number, low, guess - 1);
    } else {
      return binarySearch(number, guess + 1, high);
    }
  }
}
