using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Infrastructure.Shared
{
    /// <summary>
    /// Based on https://andrewlock.net/strongly-typed-ids-in-ef-core-using-strongly-typed-entity-ids-to-avoid-primitive-obsession-part-4/
    /// </summary>
    public class StronglyEntityIdValueConverterSelector : ValueConverterSelector
    {
        private readonly ConcurrentDictionary<(Type ModelClrType, Type ProviderClrType), ValueConverterInfo> _converters
            = new ConcurrentDictionary<(Type ModelClrType, Type ProviderClrType), ValueConverterInfo>();

        public StronglyEntityIdValueConverterSelector(ValueConverterSelectorDependencies dependencies) 
            : base(dependencies)
        {
        }

        public override IEnumerable<ValueConverterInfo> Select(Type modelClrType, Type providerClrType = null)
        {
            var baseConverters = base.Select(modelClrType, providerClrType);
            foreach (var converter in baseConverters)
            {
                yield return converter;
            }

            var underlyingModelType = UnwrapNullableType(modelClrType);
            var underlyingProviderType = UnwrapNullableType(providerClrType);

            if (underlyingProviderType is null || underlyingProviderType == typeof(String))
            {
                var isTypedIdValue = typeof(EntityId).IsAssignableFrom(underlyingModelType);
                if (isTypedIdValue)
                {
                    var converterType = typeof(EntityIdValueConverter<>).MakeGenericType(underlyingModelType);

                    yield return _converters.GetOrAdd((underlyingModelType, typeof(String)), _ =>
                    {
                        return new ValueConverterInfo(
                            modelClrType: modelClrType,
                            providerClrType: typeof(String),
                            factory: valueConverterInfo => (ValueConverter)Activator.CreateInstance(converterType, valueConverterInfo.MappingHints));
                    });
                }
            }
        }

        private static Type UnwrapNullableType(Type type)
        {
            if (type is null)
            {
                return null;
            }

            return Nullable.GetUnderlyingType(type) ?? type;
        }
    }
}