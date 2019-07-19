package fi.dy.masa.minihud.mixin;

import java.util.Map;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.world.gen.ChunkGeneratorFlat;
import net.minecraft.world.gen.structure.MapGenStructure;

@Mixin(ChunkGeneratorFlat.class)
public interface IMixinChunkGeneratorFlat
{
    @Accessor("structureGenerators")
    Map<String, MapGenStructure> getStructureGenerators();
}
