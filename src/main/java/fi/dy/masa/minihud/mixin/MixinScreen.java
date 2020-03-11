package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DataStorage;

@Mixin(net.minecraft.client.gui.screen.Screen.class)
public abstract class MixinScreen extends net.minecraft.client.gui.AbstractParentElement
{
    @Shadow protected net.minecraft.client.MinecraftClient client;

    @Inject(method = "sendMessage(Ljava/lang/String;Z)V", at = @At(
            value = "INVOKE",
            target = "Lnet/minecraft/client/network/ClientPlayerEntity;sendChatMessage(Ljava/lang/String;)V"),
            cancellable = true)
    private void onSendMessage(String msg, boolean addToChat, CallbackInfo ci)
    {
        if (DataStorage.getInstance().onSendChatMessage(this.client.player, msg))
        {
            ci.cancel();
        }
    }
}
