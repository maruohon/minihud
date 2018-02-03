package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import fi.dy.masa.itemscroller.interfaces.ISlotAccessor;
import net.minecraft.inventory.Slot;

@Mixin(Slot.class)
public abstract class MixinSlot implements ISlotAccessor
{
    @Shadow private int slotIndex;

    @Override
    public int getSlotIndex()
    {
        return this.slotIndex;
    }
}
