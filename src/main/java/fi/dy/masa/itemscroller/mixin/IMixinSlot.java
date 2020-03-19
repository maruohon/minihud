package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(net.minecraft.screen.slot.Slot.class)
public interface IMixinSlot
{
    @Accessor("index")
    int getSlotIndex();
}
