package fi.dy.masa.minihud.renderer;

import net.minecraft.client.MinecraftClient;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.Matrix4f;
import net.minecraft.util.math.Vec3d;

public interface IOverlayRenderer
{
    /**
     * Returns the camera position when the renderer was last updated
     */
    Vec3d getUpdatePosition();

    /**
     * Sets the camera position when the renderer was last updated
     */
    void setUpdatePosition(Vec3d cameraPosition);

    /**
     * Should this renderer draw anything at the moment, ie. is it enabled for example
     */
    boolean shouldRender(MinecraftClient mc);

    /**
     * Return true, if this renderer should get re-drawn/updated
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
     */
    void update(Vec3d cameraPos, Entity entity, MinecraftClient mc);

    /**
     * Draw the buffer contents to screen
     */
    void draw(MatrixStack matrixStack, Matrix4f projMatrix);

    /**
     * Allocates the OpenGL resources according to the current Video settings
     */
    void allocateGlResources();

    /**
     * Removes the OpenGL buffer allocations etc.
     */
    void deleteGlResources();
}
