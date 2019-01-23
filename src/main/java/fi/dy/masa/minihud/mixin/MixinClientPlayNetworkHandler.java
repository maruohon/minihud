package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.network.ClientPlayNetworkHandler;
import net.minecraft.client.network.packet.ChatMessageClientPacket;
import net.minecraft.client.network.packet.PlayerListHeaderClientPacket;
import net.minecraft.client.network.packet.WorldTimeUpdateClientPacket;

@Mixin(ClientPlayNetworkHandler.class)
public abstract class MixinClientPlayNetworkHandler
{
    @Inject(method = "onChatMessage", at = @At("RETURN"))
    private void onChatMessage(ChatMessageClientPacket packet, CallbackInfo ci)
    {
        DataStorage.getInstance().onChatMessage(packet.getMessage());
    }

    @Inject(method = "onWorldTimeUpdate", at = @At("RETURN"))
    private void onTimeUpdate(WorldTimeUpdateClientPacket packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().onServerTimeUpdate(packetIn.getTime());
    }

    @Inject(method = "onPlayerListHeader", at = @At("RETURN"))
    private void onHandlePlayerListHeaderFooter(PlayerListHeaderClientPacket packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().handleCarpetServerTPSData(packetIn.getFooter());
    }
}
