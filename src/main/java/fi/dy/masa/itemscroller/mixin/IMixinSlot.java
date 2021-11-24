package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.screen.slot.Slot;

@Mixin(Slot.class)
public interface IMixinSlot
{
    @Accessor("index")
    int itemscroller_getSlotIndex();
}
