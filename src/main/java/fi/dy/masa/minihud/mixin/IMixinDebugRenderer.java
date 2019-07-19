package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.client.renderer.debug.DebugRenderer;

@Mixin(DebugRenderer.class)
public interface IMixinDebugRenderer
{
    @Accessor public void setPathfindingEnabled(boolean value);
    @Accessor public void setWaterEnabled(boolean value);
    @Accessor public void setHeightMapEnabled(boolean value);
    @Accessor public void setCollisionBoxEnabled(boolean value);
    @Accessor public void setNeighborsUpdateEnabled(boolean value);
    @Accessor public void setSolidFaceEnabled(boolean value);
}
