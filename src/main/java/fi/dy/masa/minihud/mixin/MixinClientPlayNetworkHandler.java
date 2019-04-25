package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.network.ClientPlayNetworkHandler;
import net.minecraft.client.network.packet.ChatMessageS2CPacket;
import net.minecraft.client.network.packet.PlayerListHeaderS2CPacket;
import net.minecraft.client.network.packet.WorldTimeUpdateS2CPacket;

@Mixin(ClientPlayNetworkHandler.class)
public abstract class MixinClientPlayNetworkHandler
{
    @Inject(method = "onChatMessage", at = @At("RETURN"))
    private void onChatMessage(ChatMessageS2CPacket packet, CallbackInfo ci)
    {
        DataStorage.getInstance().onChatMessage(packet.getMessage());
    }

    @Inject(method = "onWorldTimeUpdate", at = @At("RETURN"))
    private void onTimeUpdate(WorldTimeUpdateS2CPacket packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().onServerTimeUpdate(packetIn.getTime());
    }

    @Inject(method = "onPlayerListHeader", at = @At("RETURN"))
    private void onHandlePlayerListHeaderFooter(PlayerListHeaderS2CPacket packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().handleCarpetServerTPSData(packetIn.getFooter());
    }
}
