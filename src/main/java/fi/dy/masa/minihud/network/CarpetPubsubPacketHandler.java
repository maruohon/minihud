package fi.dy.masa.minihud.network;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
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
import net.minecraft.world.World;
import fi.dy.masa.malilib.network.ClientPacketChannelHandler;
import fi.dy.masa.malilib.network.IClientPacketChannelHandler;
import fi.dy.masa.malilib.network.IPluginChannelHandler;
import fi.dy.masa.malilib.network.PacketSplitter;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.data.MobcapData;
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

    private static final HashSet<String> SUBSCRIPTIONS = new HashSet<>();
    private static final HashSet<String> MOB_CAP_SUBSCRIPTIONS = new HashSet<>();

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

        builder.put(NODE_SERVER_TPS,    NodeType.create(TYPE_DOUBLE, buf -> data.getTpsData().handleCarpetServerPubsubTps(buf.readDouble())));
        builder.put(NODE_SERVER_MSPT,   NodeType.create(TYPE_DOUBLE, buf -> data.getTpsData().handleCarpetServerPubsubMspt(buf.readDouble())));

        for (EnumDyeColor color : EnumDyeColor.values())
        {
            builder.put("carpet.counter." + color.getName(), NodeType.create(TYPE_LONG, buf -> {}));
        }

        MobcapData mobcapData = data.getMobcapData();

        for (DimensionType dim : DimensionType.values())
        {
            String prefix = "minecraft." + dim.getName();
            String node = prefix + ".chunk_loading.dropped_chunks.hash_size";
            builder.put(node, NodeType.create(TYPE_INT, buf -> {}));

            for (EnumCreatureType creatureType : EnumCreatureType.values())
            {
                String name = creatureType.name().toLowerCase(Locale.ROOT);
                String mobcapPrefix = prefix + ".mob_cap." + name;
                builder.put(mobcapPrefix + ".filled", NodeType.create(TYPE_INT, buf -> mobcapData.handleCarpetServerPubsubMobcap(name, false, buf.readInt())));
                builder.put(mobcapPrefix + ".total",  NodeType.create(TYPE_INT, buf -> mobcapData.handleCarpetServerPubsubMobcap(name, true, buf.readInt())));
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
        SUBSCRIPTIONS.clear();
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
            int nodeCount = 0;

            for (String node : nodes)
            {
                if (updateType == PACKET_C2S_SUBSCRIBE && SUBSCRIPTIONS.contains(node) == false)
                {
                    buf.writeString(node);
                    SUBSCRIPTIONS.add(node);
                    ++nodeCount;
                }
                else if (updateType == PACKET_C2S_UNSUBSCRIBE && SUBSCRIPTIONS.contains(node))
                {
                    buf.writeString(node);
                    SUBSCRIPTIONS.remove(node);
                    ++nodeCount;
                }
            }

            if (nodeCount > 0)
            {
                PacketSplitter.send(handler, CHANNEL_NAME, buf);
            }
        }
    }

    public static void updatePubsubSubscriptions()
    {
        World world = Minecraft.getMinecraft().world;

        if (world != null)
        {
            IClientPacketChannelHandler handler = ClientPacketChannelHandler.getInstance();
            Set<String> unsubs = new HashSet<>();
            Set<String> newsubs = new HashSet<>();

            handler.registerClientChannelHandler(INSTANCE);

            if (InfoToggle.SERVER_TPS.getBooleanValue())
            {
                newsubs.add(NODE_SERVER_TPS);
                newsubs.add(NODE_SERVER_MSPT);
            }
            else
            {
                unsubs.add(NODE_SERVER_TPS);
                unsubs.add(NODE_SERVER_MSPT);
            }

            //List<String> mobCaps = getMobcapNodeNames(world.provider.getDimensionType());

            // First unsubscribe from any previous subscriptions, plus in case the game
            // was restarted, also unsubscribe from the current dimension mob caps
            unsubs.addAll(MOB_CAP_SUBSCRIPTIONS);
            //unsubs.addAll(mobCaps);
            MOB_CAP_SUBSCRIPTIONS.clear();

            if (InfoToggle.MOB_CAPS.getBooleanValue())
            {
                List<String> mobCaps = getMobcapNodeNames(world.provider.getDimensionType());
                newsubs.addAll(mobCaps);
                unsubs.removeAll(mobCaps);
                MOB_CAP_SUBSCRIPTIONS.addAll(mobCaps);
            }

            if (unsubs.isEmpty() == false)
            {
                unsubscribe(unsubs);
            }

            if (newsubs.isEmpty() == false)
            {
                subscribe(newsubs);
            }
            else
            {
                handler.unregisterClientChannelHandler(INSTANCE);
            }
        }
    }

    private static List<String> getMobcapNodeNames(DimensionType dimensionType)
    {
        List<String> nodes = new ArrayList<>();
        String prefix = "minecraft." + dimensionType.getName() + ".mob_cap.";

        for (EnumCreatureType creatureType : EnumCreatureType.values())
        {
            String pre = prefix + creatureType.name().toLowerCase(Locale.ROOT);
            nodes.add(pre + ".filled");
            nodes.add(pre + ".total");
        }

        return nodes;
    }
}
