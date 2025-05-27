package inf112.utils;

import com.badlogic.gdx.Gdx;

public class Timer {
    private float timeRemaining;
    private boolean running;
    private float seconds;

    public void start(float seconds) {
        if (running)
            return;

        this.seconds = seconds;
        this.running = true;
        this.timeRemaining = seconds;
    }

    public void stop() {
        running = false;
    }

    public float getTimeRemaining() {
        return timeRemaining;
    }

    public void reset() {
        timeRemaining = seconds;
        running = false;
    }

    public boolean done() {
        return timeRemaining <= 0 && !running;
    }

    public boolean running() {
        return running;
    }

    public void update() {
        if (running) {
            timeRemaining -= Gdx.graphics.getDeltaTime();
            if (timeRemaining <= 0) {
                timeRemaining = 0;
                running = false;
            }
        }
    }
}
