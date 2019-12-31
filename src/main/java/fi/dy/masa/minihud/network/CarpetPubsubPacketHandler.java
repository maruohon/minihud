package fi.dy.masa.minihud.network;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Consumer;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import net.minecraft.client.Minecraft;
import net.minecraft.client.network.NetHandlerPlayClient;
import net.minecraft.entity.EnumCreatureType;
import net.minecraft.item.EnumDyeColor;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.DimensionType;
import fi.dy.masa.malilib.network.IPluginChannelHandler;
import fi.dy.masa.malilib.network.PacketSplitter;
import fi.dy.masa.minihud.util.DataStorage;
import io.netty.buffer.Unpooled;

public class CarpetPubsubPacketHandler implements IPluginChannelHandler
{
    public static final ResourceLocation CHANNEL_NAME = new ResourceLocation("carpet:pubsub");
    public static final List<ResourceLocation> CHANNELS = ImmutableList.of(CHANNEL_NAME);
    public static final CarpetPubsubPacketHandler INSTANCE = new CarpetPubsubPacketHandler();

    public static final int PACKET_C2S_SUBSCRIBE = 1;
    public static final int PACKET_C2S_UNSUBSCRIBE = 2;

    public static final int PACKET_S2C_UPDATE = 1;

    public static final int TYPE_NBT = 0;
    public static final int TYPE_STRING = 1;
    public static final int TYPE_INT = 2;
    public static final int TYPE_FLOAT = 3;
    public static final int TYPE_LONG = 4;
    public static final int TYPE_DOUBLE = 5;
    public static final int TYPE_BOOLEAN = 6;

    public static final String NODE_SERVER_TPS = "minecraft.performance.tps";
    public static final String NODE_SERVER_MSPT = "minecraft.performance.mspt";

    private static final Map<String, NodeType> NODE_TYPES;

    private static class NodeType
    {
        public final int typeId;
        public final Consumer<PacketBuffer> handler;

        public NodeType(int typeId, Consumer<PacketBuffer> handler)
        {
            this.typeId = typeId;
            this.handler = handler;
        }

        private static NodeType create(int typeId, Consumer<PacketBuffer> handler)
        {
            return new NodeType(typeId, handler);
        }
    }

    static
    {
        NODE_TYPES = registerTypes();
    }

    private static Map<String, NodeType> registerTypes()
    {
        ImmutableMap.Builder<String, NodeType> builder = ImmutableMap.builder();
        DataStorage data = DataStorage.getInstance();

        builder.put(NODE_SERVER_TPS,    NodeType.create(TYPE_DOUBLE, buf -> data.handleCarpetServerPubsubTPS(buf.readDouble())));
        builder.put(NODE_SERVER_MSPT,   NodeType.create(TYPE_DOUBLE, buf -> data.handleCarpetServerPubsubMSPT(buf.readDouble())));

        for (EnumDyeColor color : EnumDyeColor.values())
        {
            builder.put("carpet.counter." + color.getName(), NodeType.create(TYPE_LONG, buf -> {}));
        }

        for (DimensionType dim : DimensionType.values())
        {
            String prefix = "minecraft." + dim.getName();
            String node = prefix + ".chunk_loading.dropped_chunks.hash_size";
            builder.put(node, NodeType.create(TYPE_INT, buf -> {}));

            for (EnumCreatureType creatureType : EnumCreatureType.values())
            {
                String mobcapPrefix = prefix + ".mob_cap." + creatureType.name().toLowerCase(Locale.ROOT);
                builder.put(mobcapPrefix + ".filled", NodeType.create(TYPE_INT, buf -> {}));
                builder.put(mobcapPrefix + ".total", NodeType.create(TYPE_INT, buf -> {}));
            }
        }

        return builder.build();
    }

    @Override
    public List<ResourceLocation> getChannels()
    {
        return CHANNELS;
    }

    @Override
    public void onPacketReceived(PacketBuffer buf)
    {
        int id = buf.readVarInt();

        if (id == PACKET_S2C_UPDATE)
        {
            final int count = buf.readVarInt();

            for (int i = 0; i < count; ++i)
            {
                String node = buf.readString(64);
                int type = buf.readVarInt();

                if (this.readNodeData(node, type, buf) == false)
                {
                    break;
                }
            }
        }
    }

    protected boolean readNodeData(String node, int type, PacketBuffer buf)
    {
        NodeType expectedType = NODE_TYPES.getOrDefault(node, null);

        if (expectedType != null && expectedType.typeId == type)
        {
            expectedType.handler.accept(buf);
            return true;
        }

        return false;
    }

    public static void subscribe(String... nodes)
    {
        subscribe(Arrays.asList(nodes));
    }

    public static void subscribe(Collection<String> nodes)
    {
        updateSubscriptions(PACKET_C2S_SUBSCRIBE, nodes);
    }

    public static void unsubscribeAll()
    {
        unsubscribe(NODE_TYPES.keySet());
    }

    public static void unsubscribe(String... nodes)
    {
        unsubscribe(Arrays.asList(nodes));
    }

    public static void unsubscribe(Collection<String> nodes)
    {
        updateSubscriptions(PACKET_C2S_UNSUBSCRIBE, nodes);
    }

    private static void updateSubscriptions(int updateType, Collection<String> nodes)
    {
        NetHandlerPlayClient handler = Minecraft.getMinecraft().getConnection();

        if (handler != null)
        {
            PacketBuffer buf = new PacketBuffer(Unpooled.buffer());
            buf.writeVarInt(updateType);
            buf.writeVarInt(nodes.size());

            for (String node : nodes)
            {
                buf.writeString(node);
            }

            PacketSplitter.send(handler, CHANNEL_NAME, buf);
        }
    }
}
