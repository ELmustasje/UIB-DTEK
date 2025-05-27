package inf112.core.state;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import inf112.gfx.DrawableType;
import inf112.utils.Vec2;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import inf112.gfx.Drawable;
import inf112.gfx.Interactable;

class AStateTest {

    private AState state;

    @BeforeEach
    void setUp() {
        state = new AState() {
            @Override
            public void init(StateManager manager) {

            }

            @Override
            public void update() {

            }

            @Override
            public void clean() {

            }
        }; // Bruker en anonym subklasse siden AState er abstrakt
    }

    @Test
    void testStageDrawable() {
        Drawable drawable = new MockDrawable();
        state.stage(drawable);

        List<Drawable> drawables = state.drawables();
        assertTrue(drawables.contains(drawable));

        assertThrows(NullPointerException.class, () -> state.stage((Drawable) null));
    }

    @Test
    void testStageInteractable() {
        Interactable interactable = new MockInteractable();
        state.stage(interactable);

        List<Interactable> interactables = state.interactables();
        assertTrue(interactables.contains(interactable));

        List<Drawable> drawables = state.drawables();
        assertTrue(drawables.contains(interactable)); // Fordi Interactable er også Drawable i din kode

        assertThrows(NullPointerException.class, () -> state.stage((Interactable) null));
    }

    @Test
    void testStageChild() {
        AState childState = new AState() {
            @Override
            public void init(StateManager manager) {

            }

            @Override
            public void update() {

            }

            @Override
            public void clean() {

            }
        };
        Drawable childDrawable = new MockDrawable();
        Interactable childInteractable = new MockInteractable();

        childState.stage(childDrawable);
        childState.stage(childInteractable);
        state.stageChild(childState);

        assertTrue(state.drawables().contains(childDrawable));
        assertTrue(state.interactables().contains(childInteractable));
    }

    @Test
    void testAddComponent() {
        Interactable uiComponent = new MockInteractable();
        state.addComponent(uiComponent);

        assertTrue(state.interactables().contains(uiComponent));
    }

    @Test
    void testClear() {
        Drawable drawable = new MockDrawable();
        Interactable interactable = new MockInteractable();

        state.stage(drawable);
        state.stage(interactable);
        state.clear();

        assertTrue(state.drawables().isEmpty());
        assertTrue(state.interactables().isEmpty());
    }

    // Mock-klasser for testing
    private static class MockDrawable implements Drawable {
        @Override
        public DrawableType type() {
            return null;
        }

        @Override
        public float width() {
            return 0;
        }

        @Override
        public float height() {
            return 0;
        }

        @Override
        public Vec2 pos() {
            return null;
        }
    }

    private static class MockInteractable implements Interactable {

        @Override
        public boolean hover(boolean hovered) {
            return false;
        }

        @Override
        public void click(boolean clicked) {
        }

        @Override
        public DrawableType type() {
            return null;
        }

        @Override
        public float width() {
            return 0;
        }

        @Override
        public float height() {
            return 0;
        }

        @Override
        public Vec2 pos() {
            return null;
        }
    }
}
