package fi.dy.masa.minihud.network;

import net.minecraft.nbt.NbtCompound;
import net.minecraft.network.PacketByteBuf;
import net.minecraft.network.codec.PacketCodec;
import net.minecraft.network.packet.CustomPayload;

/**
 * ServuX Structures Data Provider; the actual Payload shared between MiniHUD and ServuX
 */
public record ServuxStructuresPayload(NbtCompound data) implements CustomPayload
{
    public static final Id<ServuxStructuresPayload> TYPE = new Id<>(ServuxStructuresHandler.CHANNEL_ID);
    public static final PacketCodec<PacketByteBuf, ServuxStructuresPayload> CODEC = CustomPayload.codecOf(ServuxStructuresPayload::write, ServuxStructuresPayload::new);

    public ServuxStructuresPayload(PacketByteBuf buf) { this(buf.readNbt()); }

    private void write(PacketByteBuf buf) { buf.writeNbt(data); }

    @Override
    public Id<ServuxStructuresPayload> getId() { return TYPE; }
}
