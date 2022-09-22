package fi.dy.masa.minihud.network.carpet;

import java.util.List;
import com.google.common.collect.ImmutableList;
import io.netty.buffer.Unpooled;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import fi.dy.masa.malilib.network.PacketSplitter;
import fi.dy.masa.malilib.network.message.BasePacketHandler;
import fi.dy.masa.malilib.util.game.wrap.GameUtils;
import fi.dy.masa.minihud.data.StructureStorage;

public class CarpetStructurePacketHandler extends BasePacketHandler
{
    public static final ResourceLocation CHANNEL = new ResourceLocation("carpet:client");
    public static final List<ResourceLocation> CHANNELS = ImmutableList.of(CHANNEL);
    public static final CarpetStructurePacketHandler INSTANCE = new CarpetStructurePacketHandler();

    private CarpetStructurePacketHandler()
    {
        this.registerToServer = true;
        this.usePacketSplitter = true;
    }

    @Override
    public List<ResourceLocation> getChannels()
    {
        return CHANNELS;
    }

    @Override
    public void onPacketReceived(PacketBuffer buf)
    {
        StructureStorage.INSTANCE.updateStructureDataFromCarpetServer(buf);
    }

    public void sendBoundingBoxRequest()
    {
        PacketBuffer buf = new PacketBuffer(Unpooled.buffer());
        buf.writeInt(StructureStorage.CARPET_ID_BOUNDINGBOX_MARKERS);
        PacketSplitter.send(CHANNEL, buf, GameUtils.getNetworkConnection());
    }
}
