package fi.dy.masa.minihud.mixin;

import java.util.List;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;

@Mixin(net.minecraft.client.gui.GuiScreen.class)
public abstract class MixinGuiScreen extends net.minecraft.client.gui.Gui
{
    @Shadow protected net.minecraft.client.Minecraft mc;

    @Inject(method = "getItemToolTip", at = @At("RETURN"))
    private void onGetItemTooltip(net.minecraft.item.ItemStack stack, CallbackInfoReturnable<List<String>> cir)
    {
        if (Configs.Generic.ITEM_NBT_ENABLED.getBooleanValue())
        {
            MiscUtils.getItemTooltip(stack, cir.getReturnValue());
        }
    }

    @Inject(method = "sendChatMessage(Ljava/lang/String;Z)V", at = @At(
            value = "INVOKE",
            target = "Lnet/minecraft/client/entity/EntityPlayerSP;sendChatMessage(Ljava/lang/String;)V"),
            cancellable = true)
    private void onSendMessage(String msg, boolean addToChat, CallbackInfo ci)
    {
        if (DataStorage.getInstance().onSendChatMessage(this.mc.player, msg))
        {
            ci.cancel();
        }
    }
}
