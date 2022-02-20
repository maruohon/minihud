package fi.dy.masa.minihud.network;

import java.util.List;
import com.google.common.collect.ImmutableList;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import fi.dy.masa.malilib.network.PluginChannelHandler;
import fi.dy.masa.malilib.overlay.message.MessageDispatcher;
import fi.dy.masa.minihud.MiniHUD;

public class ServuxInfoSubRegistrationPacketHandler implements PluginChannelHandler
{
    public static final ServuxInfoSubRegistrationPacketHandler INSTANCE = new ServuxInfoSubRegistrationPacketHandler();
    public static final ResourceLocation REG_CHANNEL = new ResourceLocation("servux:info_reg");

    protected static final List<ResourceLocation> CHANNELS = ImmutableList.of(REG_CHANNEL);
    protected static final int PACKET_S2C_METADATA = 1;

    @Override
    public List<ResourceLocation> getChannels()
    {
        return CHANNELS;
    }

    @Override
    public void onPacketReceived(PacketBuffer buf)
    {
        int type = buf.readVarInt();
        System.out.printf("REG packet received, type: %d\n", type);

        if (type == PACKET_S2C_METADATA)
        {
            try
            {
                ServuxInfoSubDataPacketHandler.INSTANCE.receiveMetadata(buf.readCompoundTag());
            }
            catch (Exception e)
            {
                MessageDispatcher.error("minihud.message.error.info_sub.failed_receive_metadata");
                MiniHUD.LOGGER.warn("Failed to receive info sub metadata from the server");
            }
        }
    }
}
