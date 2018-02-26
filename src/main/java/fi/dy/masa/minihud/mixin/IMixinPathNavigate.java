package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.pathfinding.PathNavigate;

@Mixin(PathNavigate.class)
public interface IMixinPathNavigate
{
    @Accessor
    float getMaxDistanceToWaypoint();
}
