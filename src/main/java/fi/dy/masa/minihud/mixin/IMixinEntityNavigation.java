package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.entity.ai.pathing.EntityNavigation;

@Mixin(EntityNavigation.class)
public interface IMixinEntityNavigation
{
    @Accessor("field_6683")
    float getMaxDistanceToWaypoint();
}
