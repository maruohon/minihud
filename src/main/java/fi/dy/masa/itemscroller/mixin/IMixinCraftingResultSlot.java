package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.inventory.CraftingInventory;
import net.minecraft.screen.slot.CraftingResultSlot;

@Mixin(CraftingResultSlot.class)
public interface IMixinCraftingResultSlot
{
    @Accessor("input")
    CraftingInventory itemscroller_getCraftingInventory();
}
