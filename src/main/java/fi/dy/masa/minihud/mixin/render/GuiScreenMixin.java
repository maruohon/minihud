package fi.dy.masa.minihud.mixin.render;

import java.util.List;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.item.ItemStack;

import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.util.MiscUtils;

@Mixin(GuiScreen.class)
public abstract class GuiScreenMixin extends Gui
{
    @Shadow protected Minecraft mc;

    @Inject(method = "getItemToolTip", at = @At("RETURN"))
    private void onGetItemTooltip(ItemStack stack, CallbackInfoReturnable<List<String>> cir)
    {
        if (Configs.Generic.ITEM_NBT_ENABLED.getBooleanValue())
        {
            MiscUtils.getItemTooltip(stack, cir.getReturnValue());
        }
    }
}
