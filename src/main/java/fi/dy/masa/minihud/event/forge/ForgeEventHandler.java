package fi.dy.masa.minihud.event.forge;

import net.minecraft.client.Minecraft;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.DebugInfoUtils;
import net.minecraftforge.client.event.ClientChatEvent;
import net.minecraftforge.client.event.ClientChatReceivedEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.player.PlayerSetSpawnEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;

public class ForgeEventHandler
{
    @SubscribeEvent
    public void onChatMessage(ClientChatReceivedEvent event)
    {
        DataStorage.getInstance().onChatMessage(event.getMessage());
    }

    @SubscribeEvent
    public void onChatMessageSend(ClientChatEvent event)
    {
        if (DataStorage.getInstance().onSendChatMessage(Minecraft.getInstance().player, event.getOriginalMessage()))
        {
            event.setCanceled(true);
        }
    }

    @SubscribeEvent
    public void onSpawnPointSet(PlayerSetSpawnEvent event)
    {
        if (event.getPlayer() == Minecraft.getInstance().player)
        {
            DataStorage.getInstance().setWorldSpawnIfUnknown(event.getNewSpawn());
        }
    }

    @SubscribeEvent
    public void onServerTickEnd(TickEvent.ServerTickEvent event)
    {
        if (event.phase == TickEvent.Phase.END)
        {
            DebugInfoUtils.onServerTickEnd(Minecraft.getInstance().getIntegratedServer());
        }
    }
}
