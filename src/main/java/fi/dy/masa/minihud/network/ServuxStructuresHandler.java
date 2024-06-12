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
import net.minecraft.util.Util;
import net.minecraft.util.math.random.Random;
import fi.dy.masa.malilib.network.IPluginClientPlayHandler;
import fi.dy.masa.malilib.network.PacketSplitter;
import fi.dy.masa.malilib.util.Constants;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.util.DataStorage;

@Environment(EnvType.CLIENT)
public abstract class ServuxStructuresHandler<T extends CustomPayload> implements IPluginClientPlayHandler<T>
{
    private final static ServuxStructuresHandler<ServuxStructuresPacket.Payload> INSTANCE = new ServuxStructuresHandler<>()
    {
        @Override
        public void receive(ServuxStructuresPacket.Payload payload, ClientPlayNetworking.Context context)
        {
            ServuxStructuresHandler.INSTANCE.receivePlayPayload(payload, context);
        }
    };
    public static ServuxStructuresHandler<ServuxStructuresPacket.Payload> getInstance() { return INSTANCE; }

    public static final Identifier CHANNEL_ID = Identifier.of("servux", "structures");

    private boolean servuxRegistered;
    private boolean payloadRegistered = false;
    private int failures = 0;
    private static final int MAX_FAILURES = 4;
    private long readingSessionKey = -1;

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

    public void decodeStructuresPacket(Identifier channel, ServuxStructuresPacket packet)
    {
        if (channel.equals(CHANNEL_ID) == false)
        {
            return;
        }
        switch (packet.getType())
        {
            case PACKET_S2C_STRUCTURE_DATA ->
            {
                if (this.readingSessionKey == -1)
                {
                    this.readingSessionKey = Random.create(Util.getMeasuringTimeMs()).nextLong();
                }

                PacketByteBuf fullPacket = PacketSplitter.receive(this, this.readingSessionKey, packet.getBuffer());

                if (fullPacket != null)
                {
                    try
                    {
                        NbtCompound nbt = fullPacket.readNbt();
                        this.readingSessionKey = -1;

                        if (nbt != null)
                        {
                            NbtList structures = nbt.getList("Structures", Constants.NBT.TAG_COMPOUND);
                            //MiniHUD.printDebug("decodeStructuresPacket(): received Structures Data of size {} (in bytes) // structures [{}]", nbt.getSizeInBytes(), structures.size());

                            DataStorage.getInstance().addOrUpdateStructuresFromServer(structures, this.servuxRegistered);
                        }
                        else
                        {
                            MiniHUD.logger.warn("decodeStructuresPacket(): Structures Data: error reading fullBuffer NBT is NULL");
                        }
                    }
                    catch (Exception e)
                    {
                        MiniHUD.logger.error("decodeStructuresPacket(): Structures Data: error reading fullBuffer [{}]", e.getLocalizedMessage());
                    }
                }
            }
            case PACKET_S2C_METADATA ->
            {
                if (DataStorage.getInstance().receiveServuxMetadata(packet.getCompound()))
                {
                    this.servuxRegistered = true;
                }
            }
            case PACKET_S2C_SPAWN_METADATA -> DataStorage.getInstance().receiveSpawnMetadata(packet.getCompound());
            default -> MiniHUD.logger.warn("decodeStructuresPacket(): received unhandled packetType {} of size {} bytes.", packet.getPacketType(), packet.getTotalSize());
        }
    }

    @Override
    public void reset(Identifier channel)
    {
        if (channel.equals(CHANNEL_ID) && this.servuxRegistered)
        {
            this.servuxRegistered = false;
            this.failures = 0;
            this.readingSessionKey = -1;
        }
    }

    public void resetFailures(Identifier channel)
    {
        if (channel.equals(CHANNEL_ID) && this.failures > 0)
        {
            this.failures = 0;
        }
    }

    @Override
    public void receivePlayPayload(T payload, ClientPlayNetworking.Context ctx)
    {
        if (payload.getId().id().equals(CHANNEL_ID))
        {
            ServuxStructuresHandler.INSTANCE.decodeStructuresPacket(CHANNEL_ID, ((ServuxStructuresPacket.Payload) payload).data());
        }
    }

    @Override
    public void encodeWithSplitter(PacketByteBuf buffer, ClientPlayNetworkHandler handler)
    {
        // NO-OP
    }

    public void encodeStructuresPacket(ServuxStructuresPacket packet)
    {
        if (ServuxStructuresHandler.INSTANCE.sendPlayPayload(new ServuxStructuresPacket.Payload(packet)) == false)
        {
            if (this.failures > MAX_FAILURES)
            {
                MiniHUD.logger.warn("encodeStructuresPacket(): encountered [{}] sendPayload failures, cancelling any Servux join attempt(s)", MAX_FAILURES);
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
