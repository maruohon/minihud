package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.block.entity.BeaconBlockEntity;

@Mixin(BeaconBlockEntity.class)
public interface IMixinBeaconBlockEntity
{
    @Accessor("level")
    int minihud_getLevel();
}
