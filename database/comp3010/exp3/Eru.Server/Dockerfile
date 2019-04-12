FROM mcr.microsoft.com/dotnet/core/aspnet:2.2-stretch-slim AS base
WORKDIR /app
EXPOSE 80
EXPOSE 443

FROM mcr.microsoft.com/dotnet/core/sdk:2.2-stretch AS build
WORKDIR /src
COPY ["Eru.Server/Eru.Server.csproj", "Eru.Server/"]
RUN dotnet restore "Eru.Server/Eru.Server.csproj"
COPY . .
WORKDIR "/src/Eru.Server"
RUN dotnet build "Eru.Server.csproj" -c Release -o /app

FROM build AS publish
RUN dotnet publish "Eru.Server.csproj" -c Release -o /app

FROM base AS final
WORKDIR /app
COPY --from=publish /app .
ENTRYPOINT ["dotnet", "Eru.Server.dll"]