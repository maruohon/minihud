package fi.dy.masa.minihud.mixin;

import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.network.ClientPlayerEntity;
import net.minecraft.text.Text;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ClientPlayerEntity.class)
public class MixinClientPlayerEntity
{
    @Final @Shadow protected net.minecraft.client.MinecraftClient client;

    @Inject(method = "sendChatMessage(Ljava/lang/String;Lnet/minecraft/text/Text;)V",
            at = @At("HEAD"),
            cancellable = true)
    private void onSendMessage(String msg, Text preview, CallbackInfo ci)
    {
        if (DataStorage.getInstance().onSendChatMessage(this.client.player, msg))
        {
            ci.cancel();
        }
    }
}
