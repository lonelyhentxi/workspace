using System;
using System.Collections.Generic;
using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using System.Text;
using Eru.Server.Configurations;
using Microsoft.Extensions.Options;
using Microsoft.IdentityModel.Tokens;

namespace Eru.Server.Services
{
    public class AuthenticationService
    {
        private readonly AuthenticationConfig _authenticationConfig;
        private readonly UrlService _urlService;
        private readonly SymmetricSecurityKey _issuerSecurityKey;
        private readonly SigningCredentials _signingCredentials;
        public readonly TimeSpan ExpirationTime;

        public AuthenticationService(IOptions<AuthenticationConfig> options, UrlService urlService)
        {
            _authenticationConfig = options.Value;
            _urlService = urlService;
            _issuerSecurityKey = BuildIssuerSigningKey(_authenticationConfig.Secret);
            _signingCredentials = new SigningCredentials(_issuerSecurityKey, SecurityAlgorithms.HmacSha256);
            ExpirationTime = TimeSpan.FromDays(_authenticationConfig.ExpiresDays);
        }

        private static SymmetricSecurityKey BuildIssuerSigningKey(string issuerSecret)
        {
            var symmetricKeyAsBase64 = issuerSecret;
            var keyByteArray = Encoding.ASCII.GetBytes(symmetricKeyAsBase64);
            return new SymmetricSecurityKey(keyByteArray);
        }

        public TokenValidationParameters BuildTokenValidationParams()
        {
            return new TokenValidationParameters
            {
                ValidateIssuerSigningKey = true,
                IssuerSigningKey = _issuerSecurityKey,
                ValidateIssuer = true,
                ValidIssuer = _authenticationConfig.Issuer,
                ValidateAudience = true,
                ValidAudience = _authenticationConfig.Audience,
                ValidateLifetime = true,
                ClockSkew = TimeSpan.Zero,
                RequireExpirationTime = true,
            };
        }


        public dynamic BuildJwtToken(IEnumerable<Claim> claims)
        {
            var now = DateTime.Now;
            var jwt = new JwtSecurityToken(
                issuer: _authenticationConfig.Issuer,
                audience: _authenticationConfig.Issuer,
                claims,
                notBefore: now,
                expires: now.Add(ExpirationTime),
                signingCredentials: _signingCredentials
            );
            var encodedJwt = new JwtSecurityTokenHandler().WriteToken(jwt);
            return new
            {
                token = encodedJwt,
                expires_in = ExpirationTime.TotalSeconds,
                token_type = "Bearer"
            };
        }
    }
}