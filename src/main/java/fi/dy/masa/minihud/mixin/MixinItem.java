package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import fi.dy.masa.minihud.util.IItemStackLimit;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;

@Mixin(Item.class)
public class MixinItem implements IItemStackLimit
{
    @Shadow
    public int getItemStackLimit() { return 0; }

    @Override
    public int getItemStackLimit(ItemStack stack)
    {
        return this.getItemStackLimit();
    }
}
