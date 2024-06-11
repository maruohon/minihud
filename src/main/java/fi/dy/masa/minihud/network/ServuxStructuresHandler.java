package fi.dy.masa.minihud.network;

import net.fabricmc.api.EnvType;
import net.fabricmc.api.Environment;
import net.fabricmc.fabric.api.client.networking.v1.ClientPlayNetworking;
import net.minecraft.client.network.ClientPlayNetworkHandler;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtList;
import net.minecraft.network.PacketByteBuf;
import net.minecraft.network.packet.CustomPayload;
import net.minecraft.util.Identifier;
import fi.dy.masa.malilib.network.IPluginClientPlayHandler;
import fi.dy.masa.malilib.util.Constants;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.util.DataStorage;

@Environment(EnvType.CLIENT)
public abstract class ServuxStructuresHandler<T extends CustomPayload> implements IPluginClientPlayHandler<T>
{
    private final static ServuxStructuresHandler<ServuxStructuresPayload> INSTANCE = new ServuxStructuresHandler<>()
    {
        @Override
        public void receive(ServuxStructuresPayload payload, ClientPlayNetworking.Context context)
        {
            ServuxStructuresHandler.INSTANCE.receivePlayPayload(payload, context);
        }
    };
    public static ServuxStructuresHandler<ServuxStructuresPayload> getInstance() { return INSTANCE; }

    public static final Identifier CHANNEL_ID = Identifier.of("servux", "structures");
    public static final int PROTOCOL_VERSION = 2;
    public static final int PACKET_S2C_METADATA = 1;
    public static final int PACKET_S2C_STRUCTURE_DATA = 2;
    public static final int PACKET_C2S_STRUCTURES_REGISTER = 3;
    public static final int PACKET_C2S_STRUCTURES_UNREGISTER = 4;
    public static final int PACKET_S2C_SPAWN_METADATA = 10;
    public static final int PACKET_C2S_REQUEST_SPAWN_METADATA = 11;

    private boolean servuxRegistered;
    private boolean payloadRegistered = false;
    private int failures = 0;
    private static final int MAX_FAILURES = 4;

    @Override
    public Identifier getPayloadChannel() { return CHANNEL_ID; }

    @Override
    public boolean isPlayRegistered(Identifier channel)
    {
        if (channel.equals(CHANNEL_ID))
        {
            return this.payloadRegistered;
        }

        return false;
    }

    @Override
    public void setPlayRegistered(Identifier channel)
    {
        if (channel.equals(CHANNEL_ID))
        {
            this.payloadRegistered = true;
        }
    }

    @Override
    public void decodeNbtCompound(Identifier channel, NbtCompound data)
    {
        //MiniHUD.logger.warn("ServuxStructuresHandler#decodeNbtCompound(): received packetType {} of size {} bytes.", data.getInt("packetType"), data.getSizeInBytes());

        switch (data.getInt("packetType"))
        {
            case PACKET_S2C_METADATA ->
            {
                if (DataStorage.getInstance().receiveServuxMetadata(data))
                {
                    this.servuxRegistered = true;
                }
            }
            case PACKET_S2C_SPAWN_METADATA -> DataStorage.getInstance().receiveSpawnMetadata(data);
            case PACKET_S2C_STRUCTURE_DATA ->
            {
                MiniHUD.printDebug("ServuxStructuresHandler#decodeNbtCompound(): received Structures Data payload of size [{}] (in bytes)", data.getSizeInBytes());

                NbtList structures = data.getList("Structures", Constants.NBT.TAG_COMPOUND);
                DataStorage.getInstance().addOrUpdateStructuresFromServer(structures, this.servuxRegistered);
            }
            default -> MiniHUD.logger.warn("ServuxStructuresHandler#decodeNbtCompound(): received unhandled packetType {} of size [{}] bytes.", data.getInt("packetType"), data.getSizeInBytes());
        }
    }

    @Override
    public void reset(Identifier channel)
    {
        if (channel.equals(CHANNEL_ID) && this.servuxRegistered)
        {
            MiniHUD.printDebug("reset() called for {}", channel.toString());

            this.servuxRegistered = false;
            this.failures = 0;
        }
    }

    public void resetFailures(Identifier channel)
    {
        if (channel.equals(CHANNEL_ID) && this.failures > 0)
        {
            MiniHUD.printDebug("resetFailures() called for {}", channel.toString());
            this.failures = 0;
        }
    }

    @Override
    public void receivePlayPayload(T payload, ClientPlayNetworking.Context ctx)
    {
        if (payload.getId().id().equals(CHANNEL_ID))
        {
            ServuxStructuresHandler.INSTANCE.decodeNbtCompound(CHANNEL_ID, ((ServuxStructuresPayload) payload).data());
        }
    }

    @Override
    public void encodeWithSplitter(PacketByteBuf buf, ClientPlayNetworkHandler handler)
    {
        // NO-OP
    }

    @Override
    public void encodeNbtCompound(NbtCompound data)
    {
        if (ServuxStructuresHandler.INSTANCE.sendPlayPayload(new ServuxStructuresPayload(data)) == false)
        {
            if (this.failures > MAX_FAILURES)
            {
                MiniHUD.logger.warn("encodeNbtCompound: encountered [{}] sendPayload failures, cancelling any Servux join attempt(s)", MAX_FAILURES);
                this.servuxRegistered = false;
                ServuxStructuresHandler.INSTANCE.unregisterPlayReceiver();
                DataStorage.getInstance().onPacketFailure();
            }
            else
            {
                this.failures++;
            }
        }
    }
}
