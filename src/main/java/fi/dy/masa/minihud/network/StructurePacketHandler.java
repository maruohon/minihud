package fi.dy.masa.minihud.network;

import java.util.List;
import com.google.common.collect.ImmutableList;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import fi.dy.masa.malilib.network.IPluginChannelHandler;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.util.DataStorage;

public class StructurePacketHandler implements IPluginChannelHandler
{
    public static final List<ResourceLocation> CHANNELS = ImmutableList.of(new ResourceLocation(LiteModMiniHud.CHANNEL_CARPET_CLIENT_NEW));
    public static final StructurePacketHandler INSTANCE = new StructurePacketHandler();

    @Override
    public List<ResourceLocation> getChannels()
    {
        return CHANNELS;
    }

    @Override
    public void onPacketReceived(PacketBuffer buf)
    {
        DataStorage.getInstance().updateStructureDataFromCarpetServer(buf);
    }
}
