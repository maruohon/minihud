package fi.dy.masa.minihud.renderer;

import net.minecraft.client.MinecraftClient;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.Vec3d;

public interface IOverlayRenderer
{
    /**
     * Returns the camera position when the renderer was last updated
     * @return
     */
    Vec3d getUpdatePosition();

    /**
     * Sets the camera position when the renderer was last updated
     * @param cameraPosition
     */
    void setUpdatePosition(Vec3d cameraPosition);

    /**
     * Should this renderer draw anything at the moment, ie. is it enabled for example
     * @return
     */
    boolean shouldRender(MinecraftClient mc);

    /**
     * Return true, if this renderer should get re-drawn/updated
     * @param entity
     * @param mc
     * @return
     */
    boolean needsUpdate(Entity entity, MinecraftClient mc);

    /**
     * Re-draw the buffer contents, if needed
     * @param cameraPos The position of the camera when the method is called.
     * The camera position should be subtracted from any world coordinates for the vertex positions.
     * During the draw() call the MatrixStack will be translated by the camera position,
     * minus the difference between the camera position during the update() call,
     * and the camera position during the draw() call.
     * @param entity The current camera entity
     * @param mc
     */
    void update(Vec3d cameraPos, Entity entity, MinecraftClient mc);

    /**
     * Draw the buffer contents to screen
     * @param x
     * @param y
     * @param z
     */
    void draw(MatrixStack matrixStack);

    /**
     * Allocates the OpenGL resources according to the current Video settings
     */
    void allocateGlResources();

    /**
     * Removes the OpenGL buffer allocations etc.
     */
    void deleteGlResources();
}
