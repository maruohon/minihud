package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.client.render.debug.DebugRenderer;

@Mixin(DebugRenderer.class)
public interface IMixinDebugRenderer
{
    @Accessor("showChunkBorder")
    public boolean getChunkBorderEnabled();
    @Accessor("showPathfinding")
    public boolean getPathfindingEnabled();
    @Accessor("showWater")
    public boolean getWaterEnabled();
    @Accessor("showHeightmap")
    public boolean getHeightMapEnabled();
    @Accessor("showVoxels")
    public boolean getCollisionBoxEnabled();
    @Accessor("showNeighborUpdates")
    public boolean getNeighborsUpdateEnabled();
    @Accessor("field_4518")
    public boolean getSolidFaceEnabled();

    @Accessor("showChunkBorder")
    public void setChunkBorderEnabled(boolean value);
    @Accessor("showPathfinding")
    public void setPathfindingEnabled(boolean value);
    @Accessor("showWater")
    public void setWaterEnabled(boolean value);
    @Accessor("showHeightmap")
    public void setHeightMapEnabled(boolean value);
    @Accessor("showVoxels")
    public void setCollisionBoxEnabled(boolean value);
    @Accessor("showNeighborUpdates")
    public void setNeighborsUpdateEnabled(boolean value);
    @Accessor("field_4518")
    public void setSolidFaceEnabled(boolean value);
}
