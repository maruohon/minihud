package fi.dy.masa.minihud.mixin.debugrenderer;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

import net.minecraft.client.renderer.debug.DebugRenderer;

@Mixin(DebugRenderer.class)
public interface DebugRendererMixin
{
    @Accessor("collisionBoxEnabled")
    void minihud_setCollisionBoxEnabled(boolean value);

    @Accessor("heightMapEnabled")
    void minihud_setHeightMapEnabled(boolean value);

    @Accessor("neighborsUpdateEnabled")
    void minihud_setNeighborsUpdateEnabled(boolean value);

    @Accessor("pathfindingEnabled")
    void minihud_setPathfindingEnabled(boolean value);

    @Accessor("solidFaceEnabled")
    void minihud_setSolidFaceEnabled(boolean value);

    @Accessor("waterEnabled")
    void minihud_setWaterEnabled(boolean value);
}
