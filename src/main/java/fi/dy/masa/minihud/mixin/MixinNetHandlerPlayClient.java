package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.event.RenderEventHandler;
import net.minecraft.client.network.NetHandlerPlayClient;
import net.minecraft.network.play.server.SPacketChat;
import net.minecraft.network.play.server.SPacketPlayerListHeaderFooter;
import net.minecraft.network.play.server.SPacketTimeUpdate;

@Mixin(NetHandlerPlayClient.class)
public class MixinNetHandlerPlayClient
{
    @Inject(method = "handleChat", at = @At("RETURN"))
    private void onChatMessage(SPacketChat packet, CallbackInfo ci)
    {
        RenderEventHandler.getInstance().onChatMessage(packet.getChatComponent());
    }

    @Inject(method = "handleTimeUpdate", at = @At("RETURN"))
    private void onTimeUpdate(SPacketTimeUpdate packetIn, CallbackInfo ci)
    {
        RenderEventHandler.getInstance().onServerTimeUpdate(packetIn.getTotalWorldTime());
    }

    @Inject(method = "handlePlayerListHeaderFooter", at = @At("RETURN"))
    private void onHandlePlayerListHeaderFooter(SPacketPlayerListHeaderFooter packetIn, CallbackInfo ci)
    {
        RenderEventHandler.getInstance().handleCarpetServerTPSData(packetIn.getFooter());
    }
}
