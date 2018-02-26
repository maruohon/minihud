package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.event.RenderEventHandler;
import net.minecraft.client.network.NetHandlerPlayClient;
import net.minecraft.network.play.server.SPacketChat;

@Mixin(NetHandlerPlayClient.class)
public class MixinNetHandlerPlayClient
{
    @Inject(method = "handleChat", at = @At("RETURN"))
    public void onChatMessage(SPacketChat packet, CallbackInfo ci)
    {
        RenderEventHandler.getInstance().onChatMessage(packet.getChatComponent());
    }
}
