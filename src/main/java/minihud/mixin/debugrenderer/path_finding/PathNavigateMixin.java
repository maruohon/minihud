package minihud.mixin.debugrenderer.path_finding;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

import net.minecraft.pathfinding.PathNavigate;

@Mixin(PathNavigate.class)
public interface PathNavigateMixin
{
    @Accessor("maxDistanceToWaypoint")
    float minihud_getMaxDistanceToWaypoint();
}
