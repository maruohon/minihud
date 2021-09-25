package fi.dy.masa.minihud.network;

import java.util.List;
import com.google.common.collect.ImmutableList;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import fi.dy.masa.malilib.network.PluginChannelHandler;
import fi.dy.masa.minihud.data.DataStorage;

public class ServuxStructurePacketHandler implements PluginChannelHandler
{
    public static final List<ResourceLocation> CHANNELS = ImmutableList.of(new ResourceLocation("servux:structure"));
    public static final ServuxStructurePacketHandler INSTANCE = new ServuxStructurePacketHandler();

    @Override
    public List<ResourceLocation> getChannels()
    {
        return CHANNELS;
    }

    @Override
    public void onPacketReceived(PacketBuffer buf)
    {
        DataStorage.getInstance().getStructureStorage().updateStructureDataFromServuxServer(buf);
    }
}
