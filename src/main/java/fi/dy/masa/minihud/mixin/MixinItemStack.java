package fi.dy.masa.minihud.mixin;

import java.util.List;
import javax.annotation.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import org.spongepowered.asm.mixin.injection.callback.LocalCapture;
import net.minecraft.block.BeehiveBlock;
import net.minecraft.client.item.TooltipContext;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.BlockItem;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.SuspiciousStewItem;
import net.minecraft.text.Text;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.util.MiscUtils;

@Mixin(ItemStack.class)
public abstract class MixinItemStack
{
    @Shadow
    public abstract Item getItem();

    @Inject(method = "getTooltip", at = @At("RETURN"), locals = LocalCapture.CAPTURE_FAILSOFT)
    private void onGetTooltip(@Nullable PlayerEntity player, TooltipContext context, CallbackInfoReturnable<List<Text>> ci, List<Text> list)
    {
        Item item = this.getItem();

        if (Configs.Generic.BEE_TOOLTIPS.getBooleanValue() &&
            item instanceof BlockItem &&
            ((BlockItem) item).getBlock() instanceof BeehiveBlock)
        {
            MiscUtils.addBeeTooltip((ItemStack) (Object) this, list);
        }

        if (Configs.Generic.STEW_TOOLTIPS.getBooleanValue() &&
            item instanceof SuspiciousStewItem)
        {
            MiscUtils.addStewTooltip((ItemStack) (Object) this, list);
        }   
    }
}
