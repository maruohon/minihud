package fi.dy.masa.minihud.renderer;

import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;

public interface IOverlayRenderer
{
    /**
     * Should this renderer draw anything at the moment, ie. is it enabled for example
     * @return
     */
    boolean shouldRender(Minecraft mc);

    /**
     * Return true, if this renderer should get re-drawn/updated
     * @param entity
     * @param mc
     * @return
     */
    boolean needsUpdate(Entity entity, Minecraft mc);

    /**
     * Re-draw the buffer contents, if needed
     * @param entity
     * @param mc
     */
    void update(Entity entity, Minecraft mc);

    /**
     * Draw the buffer contents to screen
     * @param x
     * @param y
     * @param z
     */
    void draw(double x, double y, double z);

    /**
     * Allocates the OpenGL resources according to the current Video settings
     */
    void allocateGlResources();

    /**
     * Removes the OpenGL buffer allocations etc.
     */
    void deleteGlResources();
}
