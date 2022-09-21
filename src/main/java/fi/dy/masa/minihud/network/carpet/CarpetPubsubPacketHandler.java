package fi.dy.masa.minihud.network.carpet;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import io.netty.buffer.Unpooled;
import net.minecraft.client.network.NetHandlerPlayClient;
import net.minecraft.entity.EnumCreatureType;
import net.minecraft.item.EnumDyeColor;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.DimensionType;
import net.minecraft.world.World;
import fi.dy.masa.malilib.network.ClientPacketChannelHandler;
import fi.dy.masa.malilib.network.PacketSplitter;
import fi.dy.masa.malilib.network.message.BasePacketHandler;
import fi.dy.masa.malilib.registry.Registry;
import fi.dy.masa.malilib.util.game.wrap.GameUtils;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoLineToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.data.MobCapDataHandler;
import fi.dy.masa.minihud.data.WoolCounters;

public class CarpetPubsubPacketHandler extends BasePacketHandler
{
    public static final ResourceLocation CHANNEL_NAME = new ResourceLocation("carpet:pubsub");
    public static final List<ResourceLocation> CHANNELS = ImmutableList.of(CHANNEL_NAME);
    public static final CarpetPubsubPacketHandler INSTANCE = new CarpetPubsubPacketHandler();

    public static final int PACKET_C2S_SUBSCRIBE = 1;
    public static final int PACKET_C2S_UNSUBSCRIBE = 2;

    public static final int PACKET_S2C_UPDATE = 1;

    //public static final int TYPE_NBT = 0;
    //public static final int TYPE_STRING = 1;
    public static final int TYPE_INT = 2;
    //public static final int TYPE_FLOAT = 3;
    public static final int TYPE_LONG = 4;
    public static final int TYPE_DOUBLE = 5;
    //public static final int TYPE_BOOLEAN = 6;

    public static final String NODE_SERVER_TPS = "minecraft.performance.tps";
    public static final String NODE_SERVER_MSPT = "minecraft.performance.mspt";

    protected final HashSet<String> subscriptions = new HashSet<>();
    protected final HashSet<String> perDimensionSubscriptions = new HashSet<>();
    protected final Map<String, NodeType> nodeTypes;

    protected static Map<String, NodeType> registerTypes()
    {
        ImmutableMap.Builder<String, NodeType> builder = ImmutableMap.builder();
        DataStorage data = DataStorage.getInstance();

        builder.put(NODE_SERVER_TPS,    NodeType.create(TYPE_DOUBLE, buf -> data.getTpsData().handleCarpetServerPubsubTps(buf.readDouble())));
        builder.put(NODE_SERVER_MSPT,   NodeType.create(TYPE_DOUBLE, buf -> data.getTpsData().handleCarpetServerPubsubMspt(buf.readDouble())));

        for (EnumDyeColor color : EnumDyeColor.values())
        {
            builder.put("carpet.counter." + color.getName(), NodeType.create(TYPE_LONG, buf -> data.getWoolCounters().setValue(color, buf.readLong())));
        }

        MobCapDataHandler mobCapData = data.getMobCapData();

        for (DimensionType dim : DimensionType.values())
        {
            String prefix = "minecraft." + dim.getName();
            String node = prefix + ".chunk_loading.dropped_chunks.hash_size";
            builder.put(node, NodeType.create(TYPE_INT, buf -> data.setServerDroppedChunksHashSize(buf.readInt())));

            for (EnumCreatureType creatureType : EnumCreatureType.values())
            {
                String name = creatureType.name().toLowerCase(Locale.ROOT);
                String mobCapPrefix = prefix + ".mob_cap." + name;
                builder.put(mobCapPrefix + ".filled", NodeType.create(TYPE_INT, buf -> mobCapData.putCarpetSubscribedMobCapCurrentValue(name, buf.readInt())));
                builder.put(mobCapPrefix + ".total",  NodeType.create(TYPE_INT, buf -> mobCapData.putCarpetSubscribedMobCapCapValue(name, buf.readInt())));
            }
        }

        return builder.build();
    }

    protected CarpetPubsubPacketHandler()
    {
        this.registerToServer = true;
        this.usePacketSplitter = true;
        this.nodeTypes = registerTypes();
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
        NodeType expectedType = this.nodeTypes.getOrDefault(node, null);

        if (expectedType != null && expectedType.typeId == type)
        {
            expectedType.handler.accept(buf);
            return true;
        }

        return false;
    }

    public void subscribe(String... nodes)
    {
        this.subscribe(Arrays.asList(nodes));
    }

    public void subscribe(Collection<String> nodes)
    {
        this.updateSubscriptions(PACKET_C2S_SUBSCRIBE, nodes);
    }

    public void unsubscribeAll()
    {
        this.unsubscribe(this.subscriptions);
        this.subscriptions.clear();
    }

    public void unsubscribe(String... nodes)
    {
        this.unsubscribe(Arrays.asList(nodes));
    }

    public void unsubscribe(Collection<String> nodes)
    {
        this.updateSubscriptions(PACKET_C2S_UNSUBSCRIBE, nodes);
    }

    protected void updateSubscriptions(int updateType, Collection<String> nodes)
    {
        NetHandlerPlayClient handler = GameUtils.getClient().getConnection();

        if (handler != null)
        {
            List<String> actionableNodes = new ArrayList<>();

            for (String node : nodes)
            {
                if (updateType == PACKET_C2S_SUBSCRIBE && this.subscriptions.contains(node) == false)
                {
                    actionableNodes.add(node);
                    this.subscriptions.add(node);
                }
                else if (updateType == PACKET_C2S_UNSUBSCRIBE && this.subscriptions.contains(node))
                {
                    actionableNodes.add(node);
                    this.subscriptions.remove(node);
                }
            }

            final int nodeCount = actionableNodes.size();

            if (nodeCount > 0)
            {
                if (Configs.Generic.DEBUG_MESSAGES.getBooleanValue())
                {
                    String action = updateType == PACKET_C2S_SUBSCRIBE ? "sub" : "unsub";
                    LiteModMiniHud.logger.info("Action: '{}', nodes: '{}'", action, actionableNodes);
                }

                PacketBuffer buf = new PacketBuffer(Unpooled.buffer());
                buf.writeVarInt(updateType);
                buf.writeVarInt(nodeCount);

                for (String actionableNode : actionableNodes)
                {
                    buf.writeString(actionableNode);
                }

                PacketSplitter.send(CHANNEL_NAME, buf, handler);
            }
        }
    }

    public void updatePubSubSubscriptions()
    {
        World world = GameUtils.getClientWorld();

        if (world != null)
        {
            ClientPacketChannelHandler handler = Registry.CLIENT_PACKET_CHANNEL_HANDLER;
            Set<String> newSubs = new HashSet<>();
            DimensionType dimType = world.provider.getDimensionType();

            // First unsubscribe from any previous subscriptions
            Set<String> unSubs = new HashSet<>(this.perDimensionSubscriptions);
            this.perDimensionSubscriptions.clear();

            if (InfoLineToggle.SERVER_TPS.getBooleanValue())
            {
                newSubs.add(NODE_SERVER_TPS);
                newSubs.add(NODE_SERVER_MSPT);
            }
            else
            {
                unSubs.add(NODE_SERVER_TPS);
                unSubs.add(NODE_SERVER_MSPT);
            }

            if (InfoLineToggle.CARPET_WOOL_COUNTERS.getBooleanValue())
            {
                newSubs.addAll(getWoolCounterNodeNames(true));
            }
            else
            {
                unSubs.addAll(getWoolCounterNodeNames(false));
            }

            if (InfoLineToggle.MOB_CAPS.getBooleanValue())
            {
                List<String> mobCaps = getMobCapNodeNames(dimType);
                this.addPerDimensionSubs(mobCaps, newSubs, unSubs);
            }

            if ((InfoLineToggle.CHUNK_UNLOAD_ORDER.getBooleanValue()
                 || RendererToggle.CHUNK_UNLOAD_BUCKET.isRendererEnabled()
                )
                && Configs.Generic.CHUNK_UNLOAD_BUCKET_HASH_SIZE.getBooleanValue())
            {
                String node = getDroppedChunksHashSizeNodeName(dimType);
                this.addPerDimensionSubs(Collections.singletonList(node), newSubs, unSubs);
            }

            if (unSubs.isEmpty() == false || newSubs.isEmpty() == false)
            {
                handler.registerClientChannelHandler(INSTANCE);

                if (unSubs.isEmpty() == false)
                {
                    this.unsubscribe(unSubs);
                }

                if (newSubs.isEmpty() == false)
                {
                    this.subscribe(newSubs);
                }
            }
            else
            {
                handler.unregisterClientChannelHandler(INSTANCE);
            }
        }
    }

    protected void addPerDimensionSubs(List<String> nodes, Set<String> newsubs, Set<String> unsubs)
    {
        newsubs.addAll(nodes);
        nodes.forEach(unsubs::remove);
        this.perDimensionSubscriptions.addAll(nodes);
    }

    protected static String getDroppedChunksHashSizeNodeName(DimensionType dimType)
    {
        return "minecraft." + dimType.getName() + ".chunk_loading.dropped_chunks.hash_size";
    }

    protected static List<String> getMobCapNodeNames(DimensionType dimensionType)
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

    protected static List<String> getWoolCounterNodeNames(boolean enabledOnly)
    {
        List<String> nodes = new ArrayList<>();
        WoolCounters wc = DataStorage.getInstance().getWoolCounters();

        for (EnumDyeColor color : EnumDyeColor.values())
        {
            if (enabledOnly == false || Configs.Generic.WOOL_COUNTER_ENABLE_ALL.getBooleanValue() || wc.isEnabled(color))
            {
                nodes.add("carpet.counter." + color.getName());
            }
        }

        return nodes;
    }

    public static class NodeType
    {
        public final int typeId;
        public final Consumer<PacketBuffer> handler;

        public NodeType(int typeId, Consumer<PacketBuffer> handler)
        {
            this.typeId = typeId;
            this.handler = handler;
        }

        public static NodeType create(int typeId, Consumer<PacketBuffer> handler)
        {
            return new NodeType(typeId, handler);
        }
    }
}
