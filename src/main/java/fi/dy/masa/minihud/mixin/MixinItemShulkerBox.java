package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import fi.dy.masa.minihud.config.ConfigsGeneric;
import fi.dy.masa.minihud.util.IItemStackLimit;
import fi.dy.masa.minihud.util.StackingTweaks;
import net.minecraft.block.Block;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemShulkerBox;
import net.minecraft.item.ItemStack;

@Mixin(ItemShulkerBox.class)
public class MixinItemShulkerBox extends ItemBlock implements IItemStackLimit
{
    public MixinItemShulkerBox(Block block)
    {
        super(block);
    }

    @Override
    public int getItemStackLimit(ItemStack stack)
    {
        if (ConfigsGeneric.TWEAK_SHULKERBOX_STACKING.getBooleanValue())
        {
            return StackingTweaks.shulkerBoxHasItems(stack) == false ? 64 : super.getItemStackLimit(); // FIXME
        }

        return super.getItemStackLimit();
    }
}
