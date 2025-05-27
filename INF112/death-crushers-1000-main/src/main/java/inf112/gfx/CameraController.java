package inf112.gfx;

import com.badlogic.gdx.graphics.OrthographicCamera;
import com.badlogic.gdx.math.MathUtils;

import inf112.core.Config;
import inf112.utils.Vec2;

public class CameraController {
	private float cameraVelocityX = 0f;
    private final Vec2 pos;

    private final OrthographicCamera camera;

    public CameraController(OrthographicCamera camera) {
        this.camera = camera;
        this.pos = new Vec2(0, 0);
    }

    public void update(float deltaTime, float scrollDirection, float minX, float maxX) {
		float CAMERA_ACCELERATION = 250f;
		cameraVelocityX += CAMERA_ACCELERATION * scrollDirection;
        camera.translate(cameraVelocityX * deltaTime, 0);

        camera.position.x = MathUtils.clamp(camera.position.x, minX, maxX);
		float CAMERA_FRICTION = 0.85f;
		cameraVelocityX *= CAMERA_FRICTION;
        camera.update();
    }

    public boolean accelerateTowards(Vec2 target, float speed) {
        boolean at = pos.accelerateTowards(target, speed);
        camera.translate(pos.x, pos.y);
        return at;
    }

    public void reset() {
        camera.translate(0, 0);
        pos.setZero();
        camera.position.set(Config.VIEW_WIDTH / 2.f, Config.VIEW_HEIGHT / 2.f, 0);
    }
}
