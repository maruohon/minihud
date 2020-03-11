package fi.dy.masa.itemscroller.mixin;

import net.minecraft.screen.slot.Slot;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(Slot.class)
public interface IMixinSlot
{
    @Accessor("invSlot")
    int getSlotIndex();
}
