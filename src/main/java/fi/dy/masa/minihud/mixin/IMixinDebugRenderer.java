package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.client.renderer.debug.DebugRenderer;

@Mixin(DebugRenderer.class)
public interface IMixinDebugRenderer
{
    @Accessor public boolean getChunkBorderEnabled();
    @Accessor public boolean getPathfindingEnabled();
    @Accessor public boolean getWaterEnabled();
    @Accessor public boolean getHeightMapEnabled();
    @Accessor public boolean getCollisionBoxEnabled();
    @Accessor public boolean getNeighborsUpdateEnabled();
    @Accessor public boolean getSolidFaceEnabled();

    @Accessor public void setChunkBorderEnabled(boolean value);
    @Accessor public void setPathfindingEnabled(boolean value);
    @Accessor public void setWaterEnabled(boolean value);
    @Accessor public void setHeightMapEnabled(boolean value);
    @Accessor public void setCollisionBoxEnabled(boolean value);
    @Accessor public void setNeighborsUpdateEnabled(boolean value);
    @Accessor public void setSolidFaceEnabled(boolean value);
}
